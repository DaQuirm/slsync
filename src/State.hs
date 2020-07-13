{-# LANGUAGE TemplateHaskell #-}

module State where

import           Connection      ( ClientId, Connection(..) )
import           Control.Lens    ( (?~), (.~), at )
import           Control.Lens.TH ( makeLenses )
import           Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import           Data.UUID       ( UUID )
import           Session         ( Session(..), SessionId )

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
connect connection@(Connection connectionId _ _) =
  (connections . at connectionId) ?~ connection

createSession :: Session -> State -> State
createSession session@(Session sessionId _) =
  (sessions . at sessionId) ?~ session

disconnect :: Connection -> State -> State
disconnect connection@(Connection connectionId _ _) =
  (connections . at connectionId) .~ Nothing

assignClientId :: ClientId -> Connection -> State -> State
assignClientId clientId (Connection connectionId _ connection) =
  (connections . at connectionId)
  ?~ Connection connectionId (Just clientId) connection