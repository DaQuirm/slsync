{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ServerApplication where

import           Data.List                        ( uncons )
import           Data.Functor                     ( ($>) )
import           Control.Exception                ( catch )
import           Control.Monad                    ( forever )
import           Data.Text                        ( Text, pack, splitOn )
import           Data.ByteString                  ( ByteString )
import           Data.ByteString.Lazy             ( fromStrict )
import qualified Network.WebSockets               as WebSocket
import           System.Logger                    ( Level(..), Logger )
import qualified System.Logger                    as Logger
import qualified Data.UUID.V4                     as UUID
import qualified Data.Aeson                       as Aeson
import qualified Control.Monad.Trans.State.Strict as StateT
import           Network.HTTP.Types
                 ( decodePathSegments, status200, status404 )
import           Control.Concurrent               ( MVar, putMVar, takeMVar )
import           Network.Wai
                 ( Application, Response, queryString
                 , rawPathInfo, requestMethod, responseLBS, strictRequestBody )
import           Network.Wai.Internal ( ResponseReceived(..))


import State
                 ( State, connect, disconnect, createSession, assignClientId )
import           Connection                       ( Connection(..), ConnectionId, ClientId )
import           Query                            ( success )
import           StateQuery                       ( ServiceError, StateQuery )
import           Effect
                 ( Effect(Log, Send, Respond, List), handle )
import           Session                          ( Session(..) )
import Message (Message(..))
import           GHC.Generics (Generic)


data Action =
    Connect Connection
  | AssignClientId ClientId Connection
  | Disconnect Connection WebSocket.ConnectionException
  | CreateSession Session

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
      handle logger respond $ Log Warn $ pack $ show err
    Right (effect, newState) -> do
      putMVar stateVar newState
      handle logger respond effect

stateLogic :: StateQuery Action Effect
stateLogic (Connect connection) = update $> effect
  where
    update = StateT.modify $ State.connect connection

    effect = Log Info $ "New connection: " <> pack (show connection)

stateLogic (Disconnect connection exception) = update $> effect
  where
    update = StateT.modify $ State.disconnect connection
    effect = Log Info $ "Disconnected: " <> pack (show connection) <> " "
             <> pack (show exception)

stateLogic (CreateSession session) = update $> effect
  where
    update = StateT.modify $ State.createSession session

    effect =
      List [ Respond $ responseLBS status200 [] ""
           , Log Info $ "New session: " <> pack (show session)
           ]

stateLogic (AssignClientId clientId connection) = do
  StateT.modify $ State.assignClientId clientId connection
  state <- StateT.get
  pure $ Log Info $ "Connection identity: " <> pack (show state)

notFound :: Response
notFound =
  responseLBS status404 [ ("Content-Type", "text/plain") ] "404 - Not Found"

newtype Publishers = Publishers { whitelist :: Text } deriving (Show, Generic, Aeson.FromJSON)

httpApp :: Logger -> MVar State -> Application
httpApp logger stateVar request respond = do
  requestBody <- strictRequestBody request
  case requestMethod request of
    "POST" -> case getSessionName request of
      Nothing   -> respond notFound
      Just name ->
        case Aeson.decode requestBody of
          Nothing         -> respond notFound
          Just (Publishers publishers) -> do
            updateState logger respond stateVar $ CreateSession $ Session name $ splitOn "," publishers
            pure ResponseReceived
    _ -> respond notFound
  where
    getSessionName request =
      fst <$> uncons (decodePathSegments $ rawPathInfo request)

application :: Logger -> MVar State -> WebSocket.ServerApp
application logger stateVar pending = do
  wsConnection <- WebSocket.acceptRequest pending
  WebSocket.withPingThread wsConnection 30 (pure ()) (pure ())

  connectionId <- UUID.nextRandom
  let connection = Connection connectionId Nothing wsConnection
  updateState logger dummyResponder stateVar (Connect connection)

  catch (forever (do
                    string <- fromStrict <$> (WebSocket.receiveData wsConnection :: IO ByteString)
                    Logger.trace logger $ Logger.msg $ "Received message: " <> show string
                    case Aeson.eitherDecode string of
                      Left  _ -> pure ()
                      Right (ClientId clientId) ->
                        updateState logger dummyResponder stateVar (AssignClientId (pack $ show clientId) connection)))

        (updateState logger dummyResponder stateVar . Disconnect connection)
