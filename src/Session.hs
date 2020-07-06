module Session where

import           Data.Text ( Text )

type SessionId = Text

data Session =
  Session { sessionId :: SessionId
          } deriving Show