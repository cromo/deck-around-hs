module DeckAroundCore where

import Data.Maybe (fromMaybe)
import Data.List (unionBy)

dude = Player "Dude"
dudette = Player "Dudette"
man = Player "Man"
samplePlayers = [dude, dudette, man]
sampleRound = Round
    dude
    "Mangy Susan"
    [Definition dude "A cat on a spinning platform", Definition dudette "A patchy bush", Definition man "A nasty rash"]
    [Vote dudette man, Vote man dudette]
sampleRound2 = Round
    dudette
    "Saftey Buzz"
    [Definition dudette "A drink taken as an excuse for poor decisions later", Definition man "The guard on an electric razor", Definition dude "Cutting off hair to get rid of lice"]
    [Vote man dudette, Vote dude dudette]
sampleGame = Game samplePlayers [sampleRound, sampleRound2]

-- Game play definitions

addPlayer :: GameState -> Player -> Either JoinError GameState
addPlayer (WaitingForPlayers ps) p = if elem p ps
    then Left NameInUse
    else Right $ WaitingForPlayers $ append p ps
        where append x xs = reverse $ [x] ++ reverse xs
addPlayer _ _ = Left GameInProgress

deal :: GameState -> Either DealError GameState
deal (WaitingForPlayers ps) = if length ps < 3
    then Left NotEnoughPlayers
    else Right $ Dealing (head ps) $ Game ps []
deal (EndOfRound g) = Right $ Dealing (nextDealer (players g) $ lastDealer g) g
    where lastDealer g = roundDealer $ last $ rounds g
          nextDealer ps p = head $ drop 1 $ dropWhile (p /=) $ cycle ps
deal _ = Left NotInDealableState

stateTerm :: GameState -> String -> Either TermError GameState
stateTerm (Dealing p g) t = if null t
    then Left EmptyTerm
    else Right $ Defining (Round p t [] []) g
stateTerm _ _ = Left NotInTermState

setPlayerDefinition :: GameState -> Player -> String -> Either DefinitionError GameState
setPlayerDefinition (Defining (Round dealer t ds vs) g) p d = if null d
    then Left EmptyDefinition
    else Right $ Defining (Round dealer t updatedDefinitions vs) g
        where updatedDefinitions = unionBy (\(Definition p _) (Definition q _) -> p == q) [Definition p d] ds
setPlayerDefinition _ _ _ = Left NotInDefiningState

startVoting :: GameState -> Either DefinitionError GameState
startVoting (Defining r g) = Right $ Voting r g
startVoting _ = Left NotInDefiningState

setPlayerVote :: GameState -> Player -> Player -> Either VotingError GameState
setPlayerVote (Voting (Round d t ds vs) g) vr ve =
    Right $ Voting (Round d t ds updatedVotes) g
        where updatedVotes = unionBy (\(Vote v _) (Vote u _) -> v == u) [Vote vr ve] vs
setPlayerVote _ _ _ = Left NotInVotingState

endVoting :: GameState -> Either VotingError GameState
endVoting (Voting r (Game ps rs)) = Right $ if null ws
    then EndOfRound updatedGame
    else Over updatedGame
        where updatedGame = Game ps $ append r rs
              append x xs = reverse $ [x] ++ reverse xs
              ws = winners $ scoreGame updatedGame
endVoting _ = Left NotInVotingState

-- There is no need for a transition from EndOfRound to Dealing because deal
-- already handles that case.

data JoinError
    = NameInUse
    | GameInProgress
    deriving Show

data DealError
    = NotEnoughPlayers
    | NotInDealableState
    deriving Show

data TermError
    = EmptyTerm
    | NotInTermState
    deriving Show

data DefinitionError
    = EmptyDefinition
    | NotInDefiningState
    deriving Show

data VotingError = NotInVotingState deriving Show

data RoundEndError = NotAtRoundEnd deriving Show

data GameState
    = WaitingForPlayers
        { joiningPlayers :: [Player]
        }
    | Dealing
        { dealer :: Player
        , game :: Game
        }
    | Defining
        { roundInProgress :: Round
        , game :: Game
        }
    | Voting
        { roundInProgress :: Round
        , game :: Game
        }
    | EndOfRound
        { game :: Game
        }
    | Over
        { game :: Game
        }
    deriving Show

-- Base game definitions

winners :: [(Player, Int)] -> [Player]
winners pss = map fst $ filter (\(_, s) -> 15 <= s) pss

scoreGame :: Game -> [(Player, Int)]
scoreGame (Game ps rs) = scoreRounds rs ps

scoreRounds :: [Round] -> [Player] -> [(Player, Int)]
scoreRounds rs ps = zip ps $ map sum $ map scoreAcrossRounds ps
   where rss = map ((flip scoreRound) ps) rs
         scoreAcrossRounds p = map ((flip scoreInRound) p) rss
         scoreInRound r p = fromMaybe 0 $ lookup p r

removeDealer :: Round -> [(Player, Int)] -> [(Player, Int)]
removeDealer r pss = filter notDealersScore pss
    where d = roundDealer r
          notDealersScore = \(p, _) -> d /= p

scoreRound :: Round -> [Player] -> [(Player, Int)]
scoreRound r ps = removeDealer r $ zip ps $ map (scoreForPlayerRound c v) ps
    where c = countCorrect r ps
          v = countVotes r ps

scoreForPlayerRound :: [(Player, Int)] -> [(Player, Int)] -> Player -> Int
scoreForPlayerRound c v p = fromMaybe 0 (lookup p c) + 2 * fromMaybe 0 (lookup p v)

countCorrect :: Round -> [Player] -> [(Player, Int)]
countCorrect r ps = zip ps $ map (countCorrectFor r) ps

countCorrectFor :: Round -> Player -> Int
countCorrectFor r p = length $ filter ((Vote p $ roundDealer r) ==) $ votes r

countVotes :: Round -> [Player] -> [(Player, Int)]
countVotes r ps = zip ps $ countVotesForAll r ps

countVotesForAll :: Round -> [Player] -> [Int]
countVotesForAll r ps = map (countVotesFor r) ps

countVotesFor :: Round -> Player -> Int
countVotesFor r p = length $ filter (p ==) $ map votee $ votes r

data Game = Game
    { players :: [Player]
    , rounds :: [Round]
    } deriving Show

data Player = Player
    { name :: String
    } deriving (Eq, Show)

data Definition = Definition
    { author :: Player
    , definition :: String
    } deriving Show

data Vote = Vote
    { voter :: Player
    , votee :: Player
    } deriving (Eq, Show)

data Round = Round
    { roundDealer :: Player
    , prompt :: String
    , definitions :: [Definition]
    , votes :: [Vote]
    } deriving Show