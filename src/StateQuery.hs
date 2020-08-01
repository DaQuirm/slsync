{-# LANGUAGE NamedFieldPuns #-}

module StateQuery where

import           Control.Lens                     ((^.))
import qualified Control.Monad.Trans.State.Strict as StateT
import qualified Data.Map.Strict                  as Map

import           Connection                       (Connection (..),
                                                   ConnectionId)
import           Query                            (Query, failure, success)
import           Session                          (PublisherPolicy (..),
                                                   Session (..), SessionId)
import           State                            (State, connections, sessions)
import           Subscription                     (Subscription (..))

data ServiceError
  = SomethingWentWrong
  | ConnectionNotFound
  | UnidentifiedClient
  | SessionNotFound
  | NotAPublisher
  deriving ( Show )

type StateQuery a b = Query State ServiceError a b

getConnection :: StateQuery ConnectionId Connection
getConnection connectionId = do
  state <- StateT.get
  case Map.lookup connectionId (state ^. connections) of
    Nothing         -> failure ConnectionNotFound
    Just connection -> success connection

canPublish :: StateQuery Connection ()
canPublish (Connection _ Nothing _ _) = failure UnidentifiedClient
canPublish (Connection _ (Just (Subscription {clientId})) sessionId _) = do
  state <- StateT.get
  case Map.lookup sessionId (state ^. sessions) of
    Nothing -> failure SessionNotFound
    Just Session{publisherPolicy} -> case publisherPolicy of
      AllowAll -> success ()
      Whitelist publishers
        | clientId `elem` publishers -> success ()
        | otherwise -> failure NotAPublisher

sessionConnections :: StateQuery SessionId [Connection]
sessionConnections sessionId = do
  state <- StateT.get
  success $ filter (\(Connection _ _ sId _) -> sId == sessionId) $
    map snd (Map.toList $ state ^. connections)
