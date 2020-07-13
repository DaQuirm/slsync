module Connection where

import           Data.UUID          ( UUID )
import qualified Network.WebSockets as WebSockets
import Data.Text (Text)

type ConnectionId = UUID
type ClientId = Text

data Connection = Connection ConnectionId (Maybe ClientId) WebSockets.Connection

instance Eq Connection where
  (==) (Connection idA _ _) (Connection idB _ _) = idA == idB

instance Ord Connection where
  compare (Connection idA _ _) (Connection idB _ _) = compare idA idB

instance Show Connection where
  show (Connection connectionId clientId _) = show connectionId <> " " <> show clientId
