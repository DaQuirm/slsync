{-# LANGUAGE TemplateHaskell #-}

module State where

import           Connection      ( Connection(..) )
import           Control.Lens    ( (?~), at )
import           Control.Lens.TH ( makeLenses )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.UUID       ( UUID )
import           Session         ( Session(..), SessionId )

data State =
  State { _connections :: Map UUID Connection
        , _sessions    :: Map SessionId Session
        }

makeLenses ''State

empty :: State
empty =
  State { _sessions    = Map.empty
        , _connections = Map.empty
        }

connect :: Connection -> State -> State
connect connection@(Connection connectionId _) =
  (connections . at connectionId) ?~ connection

createSession :: Session -> State -> State
createSession session@(Session sessionId) =
  (sessions . at sessionId) ?~ session
