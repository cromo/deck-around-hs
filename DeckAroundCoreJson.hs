module DeckAroundCoreJson where

import Data.Aeson (ToJSON(toJSON),object,Value(String),(.=))
--import qualified Data.Text.Lazy as LT
import qualified Data.Text as T

import DeckAroundCore

instance ToJSON Player where
  toJSON (Player name) = String $ T.pack name