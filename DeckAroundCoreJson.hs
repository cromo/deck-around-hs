{-# LANGUAGE OverloadedStrings #-}

module DeckAroundCoreJson where

import Control.Applicative ((<$>),(<*>))
import Data.Aeson (ToJSON(toJSON),FromJSON(parseJSON),object,Value(Object,String),(.=),(.:))
import Data.Aeson.Types (Pair,Parser)
import qualified Data.Text as T

import DeckAroundCore

voterKey = "voter"
voteeKey = "votee"
authorKey = "author"
definitionKey = "definition"
dealerKey = "dealer"
promptKey = "prompt"
definitionsKey = "definitions"
votesKey = "votes"
playersKey = "players"
roundsKey = "rounds"
phaseKey = "phase"
gameKey = "game"
roundInProgressKey = "roundInProgress"

waitingForPlayersPhase = "WaitingForPlayers"
dealingPhase = "Dealing"
definingPhase = "Defining"
votingPhase = "Voting"
endOfRoundPhase = "EndOfRound"
overPhase = "Over"

instance ToJSON Player where
    toJSON (Player name) = String $ T.pack name

instance ToJSON Vote where
    toJSON (Vote voter votee) = object [voterKey .= voter, voteeKey .= votee]

instance ToJSON Definition where
    toJSON (Definition a d) = object [authorKey .= a, definitionKey .= d]

instance ToJSON Round where
    toJSON (Round d p ds vs) = object
        [ dealerKey .= d
        , promptKey .= p
        , definitionsKey .= ds
        , votesKey .= vs
        ]

instance ToJSON Game where
    toJSON (Game ps rs) = object [playersKey .= ps, roundsKey .= rs]

phase :: String -> Pair
phase p = phaseKey .= p

instance ToJSON GameState where
    toJSON (WaitingForPlayers ps) = object
        [ phase waitingForPlayersPhase
        , playersKey .= ps
        ]
    toJSON (Dealing d g) = object
        [ phase dealingPhase
        , dealerKey .= d
        , gameKey .= g
        ]
    toJSON (Defining r g) = object
        [ phase definingPhase
        , roundInProgressKey .= r
        , gameKey .= g
        ]
    toJSON (Voting r g) = object
        [ phase votingPhase
        , roundInProgressKey .= r
        , gameKey .= g
        ]
    toJSON (EndOfRound g) = object [phase endOfRoundPhase, gameKey .= g]
    toJSON (Over g) = object [phase overPhase, gameKey .= g]

instance FromJSON Player where
    parseJSON s@(String _) = Player <$> parseJSON s

instance FromJSON Vote where
    parseJSON (Object v) =
        Vote <$> (Player <$> v .: voterKey) <*> (Player <$> v .: voteeKey)

instance FromJSON Definition where
    parseJSON (Object d) =
        Definition <$> (Player <$> d .: authorKey) <*> d .: definitionKey

instance FromJSON Round where
    parseJSON (Object r) = do
        d <- Player <$> r .: dealerKey
        p <- r .: promptKey >>= parseJSON
        ds <- r .: definitionsKey >>= parseJSON
        vs <- r .: votesKey >>= parseJSON
        return $ Round d p ds vs

instance FromJSON Game where
    parseJSON (Object g) = do
        ps <- g .: playersKey >>= parseJSON
        rs <- g .: roundsKey >>= parseJSON
        return $ Game ps rs

instance FromJSON GameState where
    parseJSON (Object gs) = do
        phase <- gs .: phaseKey >>= parseJSON
        case phase of
            p | p == waitingForPlayersPhase ->
                WaitingForPlayers <$> gs .: playersKey
              | p == dealingPhase -> do
                d <- gs .: dealerKey >>= parseJSON
                g <- gs .: gameKey >>= parseJSON
                return $ Dealing d g
              | p == definingPhase ->
                Defining <$> gs .: roundInProgressKey <*> gs .: gameKey
              | p == votingPhase ->
                Voting <$> gs .: roundInProgressKey <*> gs .: gameKey
              | p == endOfRoundPhase -> EndOfRound <$> gs .: gameKey
              | p == overPhase -> Over <$> gs .: gameKey