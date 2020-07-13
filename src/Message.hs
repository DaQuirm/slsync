module Message where

import           Data.ByteString.Lazy ( ByteString )
import Data.Aeson (FromJSON(..), Value(..), encode)

data Message
  = ClientId Int
  | ProtocolMessage ByteString
  deriving Show

instance FromJSON Message where
    parseJSON number@(Number _) = ClientId <$> parseJSON number
    parseJSON message = pure $ ProtocolMessage $ encode message

