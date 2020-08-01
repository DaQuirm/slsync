{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Subscription where

import           Data.Aeson           (FromJSON (..), ToJSON, Value (..),
                                       encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import           Data.Word            (Word8)
import           GHC.Generics         (Generic)

type ClientId = Text

data Subscription = Subscription
  { clientId      :: ClientId
  , prefixFilters :: [[Word8]]
  }
  deriving (Show, Generic, FromJSON, ToJSON)

