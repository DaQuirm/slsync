{-# LANGUAGE OverloadedStrings #-}

module WebSocketServer ( startServer ) where

import           Control.Concurrent             ( newMVar )
import           Data.Monoid                    ( (<>) )
import           Network.Wai                    ( Middleware )
import qualified Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets ( websocketsOr )
import           Network.Wai.Middleware.Cors
                 ( CorsResourcePolicy(..), cors, simpleCorsResourcePolicy
                 , simpleHeaders )
import qualified Network.WebSockets             as WebSocket
import qualified System.Logger                  as Logger
import qualified System.Envy                    as Envy
import qualified Data.ByteString.Char8          as ByteString
import           System.Logger                  ( Level(..), Logger )
import           System.Directory               ( createDirectoryIfMissing )

import           State                          ( State )
import qualified State
import           ServerApplication              ( application, httpApp )
import           Config                         ( Config(Config) )

corsMiddleware :: Middleware
corsMiddleware =
  cors (const $
        Just simpleCorsResourcePolicy { corsRequestHeaders = simpleHeaders
                                      })

startServer :: IO ()
startServer = do
  envConfig <- Envy.decodeEnv :: IO (Either String Config)
  case envConfig of
    Left err -> putStrLn $ "Config error:" <> err
    Right (Config port logLevel) -> do
      let infoMessage = "Listening on port " <> show port
      putStrLn infoMessage
      stateVar <- newMVar State.empty
      createDirectoryIfMissing True "logs"
      let setLogLevel = Logger.setLogLevel logLevel
          setOutput   = Logger.setOutput $ Logger.Path "logs/slsync.log"
          settings    = setLogLevel . setOutput $ Logger.defSettings
      logger <- Logger.new settings
      Logger.info logger $ Logger.msg infoMessage
      Warp.run port $ corsMiddleware $
        websocketsOr WebSocket.defaultConnectionOptions
                     (application logger stateVar)
                     (httpApp logger stateVar)
