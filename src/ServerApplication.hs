{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ServerApplication where

import           Control.Concurrent               (MVar, putMVar, readMVar,
                                                   takeMVar)
import           Control.Exception                (catch)
import           Control.Lens                     ((^.))
import           Control.Monad                    (forever)
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Aeson                       as Aeson
import           Data.ByteString.Lazy             (ByteString, fromStrict,
                                                   isPrefixOf, pack)
import           Data.Functor                     (($>))
import           Data.List                        (uncons)
import qualified Data.Map.Strict                  as Map
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.UUID.V4                     as UUID
import           Network.HTTP.Types               (decodePathSegments,
                                                   status200, status302,
                                                   status404)
import           Network.Wai                      (Application, Response,
                                                   queryString, rawPathInfo,
                                                   requestMethod, responseLBS,
                                                   strictRequestBody)
import           Network.Wai.Internal             (ResponseReceived (..))
import qualified Network.WebSockets               as WebSocket
import           System.Logger                    (Level (..), Logger)
import qualified System.Logger                    as Logger


import           Connection                       (Connection (..),
                                                   ConnectionId)
import           Effect                           (Effect (List, Log, Respond, Send),
                                                   handle)
import           GHC.Generics                     (Generic)
import           Query                            (success)
import           Session                          (Session (..), fromText)
import           State                            (State, assignSubscription,
                                                   connect, createSession,
                                                   disconnect, sessions)
import           StateQuery                       (ServiceError, StateQuery,
                                                   canPublish, getConnection,
                                                   sessionConnections)
import           Subscription                     (Subscription (..))


data Action =
    Connect Connection
  | Disconnect Connection WebSocket.ConnectionException
  | CreateSession Session
  | Subscribe Subscription ConnectionId
  | Broadcast ByteString ConnectionId

dummyResponder :: Response -> IO ResponseReceived
dummyResponder _ = pure ResponseReceived

updateState :: Logger
            -> (Response -> IO ResponseReceived)
            -> MVar State
            -> Action
            -> IO ()
updateState logger respond stateVar action = do
  state <- takeMVar stateVar
  case StateT.runStateT (stateLogic action) state of
    Left err -> do
      putMVar stateVar state
      handle logger respond $ Log Warn $ Text.pack $ show err
    Right (effect, newState) -> do
      putMVar stateVar newState
      handle logger respond effect

stateLogic :: StateQuery Action Effect
stateLogic (Connect connection) = update $> effect
  where
    update = StateT.modify $ State.connect connection

    effect = Log Info $ "New connection: " <> Text.pack (show connection)

stateLogic (Disconnect connection exception) = update $> effect
  where
    update = StateT.modify $ State.disconnect connection
    effect = Log Info $ "Disconnected: " <> Text.pack (show connection) <> " "
             <> Text.pack (show exception)

stateLogic (CreateSession session) = update $> effect
  where
    update = StateT.modify $ State.createSession session

    effect =
      List [ Respond $ responseLBS status200 [] ""
           , Log Info $ "New session: " <> Text.pack (show session)
           ]

stateLogic (Subscribe subscription@Subscription{} connectionId) = do
  connection <- getConnection connectionId
  StateT.modify $ State.assignSubscription subscription connection
  pure $ Log Info $ "Connection subscription: "  <> Text.pack (show connectionId) <> Text.pack (show subscription) <> " "

stateLogic (Broadcast message connectionId) = do
  connection@(Connection _ _ sessionId _) <- getConnection connectionId
  canPublish connection
  connections <- sessionConnections sessionId
  let
    recipientFilter (Connection _ mSubscription _ _) =
      case mSubscription of
        Just (Subscription _ prefixFilters) ->
          all (\prefixFilter -> not $ (pack prefixFilter) `isPrefixOf` message) prefixFilters
        Nothing -> False
    recipients = filter recipientFilter connections
   in pure $ List $ flip Send message <$> recipients

notFound :: Response
notFound =
  responseLBS status404 [ ("Content-Type", "text/plain") ] "404 - Not Found"

newtype Publishers = Publishers
  { whitelist :: Text }
  deriving (Show, Generic, Aeson.FromJSON)

httpApp :: Logger -> MVar State -> Application
httpApp logger stateVar request respond = do
  requestBody <- strictRequestBody request
  case requestMethod request of
    "POST" -> case getSessionId request of
      Nothing   -> respond notFound
      Just sessionId -> do
        state <- readMVar stateVar
        case Map.lookup sessionId $ state ^. sessions of
          Nothing -> case Aeson.decode requestBody of
            Nothing                      -> respond notFound
            Just (Publishers publishers) -> do
              updateState logger respond stateVar $ CreateSession $ Session sessionId $ fromText publishers
              pure ResponseReceived
          Just _ -> respond $ responseLBS status302 [] "Session already exists"

    _ -> respond notFound
  where
   getSessionId request =
      fst <$> uncons (decodePathSegments $ rawPathInfo request)

application :: Logger -> MVar State -> WebSocket.ServerApp
application logger stateVar pending = do
  let reqPath = WebSocket.requestPath $ WebSocket.pendingRequest pending

  case fst <$> uncons (decodePathSegments reqPath) of
    Nothing -> WebSocket.rejectRequest pending "Error: No session id in the connection URL"
    Just sessionId -> do
      wsConnection <- WebSocket.acceptRequest pending
      WebSocket.withPingThread wsConnection 30 (pure ()) (pure ())

      connectionId <- UUID.nextRandom
      let connection = Connection connectionId Nothing sessionId wsConnection
      updateState logger dummyResponder stateVar (Connect connection)

      catch (forever (do
                        string <- fromStrict <$> WebSocket.receiveData wsConnection :: IO ByteString
                        Logger.trace logger $ Logger.msg $ "Received message: " <> show string
                        case Aeson.eitherDecode string of
                          Right (subscription@Subscription{}) ->
                            updateState logger dummyResponder stateVar (Subscribe subscription connectionId)
                          _ ->
                            updateState logger dummyResponder stateVar (Broadcast string connectionId)
                          ))

            (updateState logger dummyResponder stateVar . Disconnect connection)
