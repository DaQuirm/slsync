module Connection where

import           Data.Text          (Text)
import           Data.UUID          (UUID)
import qualified Network.WebSockets as WebSockets

import           Session            (SessionId)
import           Subscription       (Subscription)

type ConnectionId = UUID

data Connection = Connection ConnectionId (Maybe Subscription) SessionId WebSockets.Connection

instance Eq Connection where
  (==) (Connection idA _ _ _) (Connection idB _ _ _) = idA == idB

instance Ord Connection where
  compare (Connection idA _ _ _) (Connection idB _ _ _) = compare idA idB

instance Show Connection where
  show (Connection connectionId clientId sessionId _) = show connectionId <> " " <> show clientId <> " " <> show sessionId
