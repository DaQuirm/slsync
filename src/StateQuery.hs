{-# LANGUAGE NamedFieldPuns #-}

module StateQuery where

import qualified Data.Map.Strict as Map
import qualified Control.Monad.Trans.State.Strict as StateT
import Control.Lens ((^.))

import           Query      ( Query, failure, success )
import           State      ( State, sessions, connections )
import           Connection ( Connection(..), ConnectionId )
import Session (Session(..), SessionId)

data ServiceError = SomethingWentWrong | ConnectionNotFound | UnidentifiedClient | SessionNotFound | NotAPublisher
  deriving ( Show )

type StateQuery a b = Query State ServiceError a b

getConnection :: StateQuery ConnectionId Connection
getConnection connectionId = do
  state <- StateT.get
  case Map.lookup connectionId (state ^. connections) of
    Nothing -> failure ConnectionNotFound
    Just connection -> success connection

canPublish :: StateQuery Connection ()
canPublish (Connection _ Nothing _ _) = failure UnidentifiedClient
canPublish (Connection _ (Just clientId) sessionId _) = do
  state <- StateT.get
  case Map.lookup sessionId (state ^. sessions) of
    Nothing -> failure SessionNotFound
    Just Session {publishers} | clientId `elem` publishers -> success ()
                              | otherwise -> failure NotAPublisher

sessionConnections :: StateQuery SessionId [Connection]
sessionConnections sessionId = do
  state <- StateT.get
  success $ filter (\(Connection _ _ sId _) -> sId == sessionId) $ map snd (Map.toList $ state ^. connections)
