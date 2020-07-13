module Session where

import           Data.Text ( Text )

type SessionId = Text
type ClientId = Text

data Session =
  Session { sessionId  :: SessionId
          , publishers :: [ClientId]
          } deriving Show