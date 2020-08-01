{-# LANGUAGE TemplateHaskell #-}

module State where

import           Connection      (Connection (..))
import           Control.Lens    (at, (.~), (?~))
import           Control.Lens.TH (makeLenses)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.UUID       (UUID)

import           Session         (Session (..), SessionId)
import           Subscription    (Subscription)

data State =
  State { _connections :: Map UUID Connection
        , _sessions    :: Map SessionId Session
        } deriving Show

makeLenses ''State

empty :: State
empty =
  State { _sessions    = Map.empty
        , _connections = Map.empty
        }

connect :: Connection -> State -> State
connect connection@(Connection connectionId _ _ _) =
  (connections . at connectionId) ?~ connection

createSession :: Session -> State -> State
createSession session@(Session sessionId _) =
  (sessions . at sessionId) ?~ session

disconnect :: Connection -> State -> State
disconnect connection@(Connection connectionId _ _ _) =
  (connections . at connectionId) .~ Nothing

assignSubscription :: Subscription -> Connection -> State -> State
assignSubscription clientId (Connection connectionId _ sessionId connection) =
  (connections . at connectionId)
  ?~ Connection connectionId (Just clientId) sessionId connection
