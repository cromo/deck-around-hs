{-# LANGUAGE OverloadedStrings #-}

module DeckAroundCoreJson where

import Data.Aeson (ToJSON(toJSON),object,Value(String),(.=))
import qualified Data.Text as T

import DeckAroundCore

instance ToJSON Player where
    toJSON (Player name) = String $ T.pack name

instance ToJSON Vote where
    toJSON (Vote voter votee) = object ["voter" .= voter, "votee" .= votee]

instance ToJSON Definition where
    toJSON (Definition a d) = object ["author" .= a, "definition" .= d]

instance ToJSON Round where
    toJSON (Round d p ds vs) = object
        [ "dealer" .= d
        , "prompt" .= p
        , "definitions" .= ds
        , "votes" .= vs
        ]

instance ToJSON Game where
    toJSON (Game ps rs) = object ["players" .= ps, "rounds" .= rs]