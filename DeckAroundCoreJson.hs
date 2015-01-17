{-# LANGUAGE OverloadedStrings #-}

module DeckAroundCoreJson where

import Control.Applicative ((<$>),(<*>))
import Data.Aeson (ToJSON(toJSON),FromJSON(parseJSON),object,Value(Object,String),(.=),(.:))
import Data.Aeson.Types (Pair)
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

phase :: String -> Pair
phase p = "phase" .= p

instance ToJSON GameState where
    toJSON (WaitingForPlayers ps) = object
        --[ phase .= String "WaitingForPlayers"
        [ phase "WaitingForPlayers"
        , "players" .= ps
        ]
    toJSON (Dealing d g) = object
        [ phase "Dealing"
        , "dealer" .= d
        , "game" .= g
        ]
    toJSON (Defining r g) = object
        [ phase "Defining"
        , "roundInProgress" .= r
        , "game" .= g
        ]
    toJSON (Voting r g) = object
        [ phase "Voting"
        , "roundInProgress" .= r
        , "game" .= g
        ]
    toJSON (EndOfRound g) = object [phase "EndOfRound", "game" .= g]
    toJSON (Over g) = object [phase "Over", "game" .= g]

instance FromJSON Player where
    parseJSON s@(String _) = Player <$> parseJSON s

instance FromJSON Vote where
    parseJSON (Object v) =
        Vote <$> (Player <$> v .: "voter") <*> (Player <$> v .: "votee")

instance FromJSON Definition where
    parseJSON (Object d) =
        Definition <$> (Player <$> d .: "author") <*> d .: "definition"

instance FromJSON Round where
    parseJSON (Object r) = do
        d <- Player <$> r .: "dealer"
        p <- r .: "prompt" >>= parseJSON
        ds <- r .: "definitions" >>= parseJSON
        vs <- r .: "votes" >>= parseJSON
        return $ Round d p ds vs

instance FromJSON Game where
    parseJSON (Object g) = do
        ps <- g .: "players" >>= parseJSON
        rs <- g .: "rounds" >>= parseJSON
        return $ Game ps rs