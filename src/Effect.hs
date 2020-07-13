module Effect where

import           Data.ByteString    ( ByteString )
import           Data.Monoid        ( (<>) )
import           Data.Text          ( Text )
import           Data.Functor ( ($>) )
import qualified Network.WebSockets as WebSocket
import           System.Logger      ( Level, Logger )
import qualified System.Logger      as Logger
import           Data.Foldable      ( traverse_ )
import           Network.Wai        (Response, ResponseReceived)

import           Connection         ( Connection(Connection) )

data Effect =
    Log Level Text
  | Send Connection ByteString
  | Respond Response
  | List [Effect]

handle :: Logger -> (Response -> IO ResponseReceived) -> Effect -> IO ()
handle logger _ (Log level string) =
  Logger.log logger level $ Logger.msg string

handle _ _ (Send (Connection _ _ connection) string) =
  WebSocket.sendTextData connection string

handle _ respond (Respond response) = respond response $> ()

handle logger respond (List effects) = traverse_ (handle logger respond) effects
