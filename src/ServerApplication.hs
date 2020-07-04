{-# LANGUAGE OverloadedStrings #-}

module ServerApplication where

import           Data.Functor                     ( ($>) )
import           Control.Exception                ( catch )
import           Control.Monad                    ( forever )
import           Data.Text                        ( pack )
import           Data.ByteString                  ( ByteString )
import qualified Network.WebSockets               as WebSocket
import           System.Logger                    ( Level(..), Logger )
import qualified System.Logger                    as Logger
import qualified Data.UUID.V4                     as UUID
import qualified Data.Aeson                       as Aeson
import qualified Control.Monad.Trans.State.Strict as StateT
import           Control.Concurrent               ( MVar, putMVar, takeMVar )

import           State                            ( State, connect )
import           Connection                       ( Connection(..) )
import           Query                            ( success )
import           StateQuery                       ( ServiceError, StateQuery )
import           Effect
                 ( Effect(Log, Send, List), handle )

data Action =
  Connect Connection | Disconnect Connection WebSocket.ConnectionException

updateState :: Logger -> MVar State -> Action -> IO ()
updateState logger stateVar action = do
  state <- takeMVar stateVar
  case StateT.runStateT (stateLogic action) state of
    Left err -> do
      putMVar stateVar state
      handle logger $ Log Warn $ pack $ show err
    Right (effect, newState) -> do
      putMVar stateVar newState
      handle logger effect

stateLogic :: StateQuery Action Effect
stateLogic (Connect connection) = update $> effect
  where
    update = StateT.modify $ State.connect connection

    effect = Log Info $ "New connection: " <> pack (show connection)

stateLogic (Disconnect connection exception) =
  success $ Log Info $ "Disconnected: " <> pack (show connection) <> " "
  <> pack (show exception)

application :: Logger -> MVar State -> WebSocket.ServerApp
application logger stateVar pending = do
  wsConnection <- WebSocket.acceptRequest pending
  WebSocket.withPingThread wsConnection 30 (pure ()) (pure ())

  connectionId <- UUID.nextRandom
  let connection = Connection connectionId wsConnection
  updateState logger stateVar (Connect connection)

  catch (forever (do
                    string <- WebSocket.receiveData wsConnection :: IO ByteString
                    Logger.trace logger $ Logger.msg $
                      "Received message: " <> string))
        (updateState logger stateVar . Disconnect connection)
