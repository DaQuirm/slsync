{-# LANGUAGE OverloadedStrings #-}

module Session where

import           Data.Text ( Text, splitOn )

type SessionId = Text

type ClientId = Text

data Session
  = Session { sessionId       :: SessionId
            , publisherPolicy :: PublisherPolicy
            }
  deriving Show

data PublisherPolicy
  = AllowAll
  | Whitelist [ClientId]
  deriving Show

fromText :: Text -> PublisherPolicy
fromText "*"    = AllowAll
fromText string = Whitelist $ splitOn "," string
