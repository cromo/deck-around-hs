{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.HTTP.Types.Status (badRequest400)
import Network.Wai.Middleware.StreamFile (streamFile)

import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import Control.Monad (join,when)
import Data.Maybe (isNothing)
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8,decodeUtf8)
import Data.Aeson (decode,encode,ToJSON(toJSON),object,(.=))
import Web.Cookie
import qualified Database.Redis as R

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Map as M

import DeckAroundCore
import DeckAroundCoreJson

main = do
    conn <- R.connect redisConnectInfo
    scotty 3000 $ do
        middleware streamFile

        index conn
        getStatus conn
        joinGame conn
        startRound conn
        setPrompt conn
        define conn
        moveToVote conn
        vote conn
        endRound conn
        reset conn

        playerClient

redisConnectInfo :: R.ConnectInfo
redisConnectInfo = R.defaultConnectInfo {R.connectHost = "redis"}

saveGame :: ToJSON a => R.Connection -> a -> ActionM (Either R.Reply R.Status)
saveGame r gs = runRedis r $ R.set "game" $ BSL.toStrict $ encode gs

loadGame :: R.Connection -> ActionM GameState
loadGame r = do
    game <- runRedis r $ R.get "game"
    return $ case game of
        Right (Just result) -> case decode $ BSL.fromStrict result of
            Just gs -> gs
            Nothing -> WaitingForPlayers []
        Right Nothing -> WaitingForPlayers []
        Left _ -> WaitingForPlayers []

index :: R.Connection -> ScottyM ()
index r = get "/" $ do
    gs <- loadGame r
    saveGame r $ gs
    --json gs
    setContentType htmlUtf8
    file "player.html"

playerClient :: ScottyM ()
playerClient = get "/player.js" $ do
    setContentType javascript
    file "player.js"

getStatus :: R.Connection -> ScottyM ()
getStatus r = get "/status" $ do
    gs <- loadGame r
    json gs

joinGame :: R.Connection -> ScottyM ()
joinGame r = post "/join" $ do
    name <- param "name"
    game <- loadGame r
    game <- return $ addPlayer game $ Player name
    case game of
        Left NameInUse -> errorJson "name already in use"
        Left GameInProgress -> errorJson "game already in progress"
        Right gs -> do
            saveGame r gs
            json gs

startRound :: R.Connection -> ScottyM ()
startRound r = post "/deal" $ do
    game <- loadGame r
    game <- return $ deal game
    case game of
        Left NotEnoughPlayers -> errorJson "not enough players"
        Left NotInDealableState -> errorJson "can't deal right now"
        Right gs -> do
            saveGame r gs
            json gs

setPrompt :: R.Connection -> ScottyM ()
setPrompt r = post "/prompt" $ do
    term <- param "prompt"
    game <- loadGame r
    game <- return $ stateTerm game term
    case game of
        Left EmptyTerm -> errorJson "term cannot be empty"
        Left NotInTermState -> errorJson "can't set term right now"
        Right gs -> do
            saveGame r gs
            json gs

define :: R.Connection -> ScottyM ()
define r = post "/define" $ do
    name <- param "name"
    definition <- param "definition"
    game <- loadGame r
    game <- return $ setPlayerDefinition game (Player name) definition
    case game of
        Left EmptyDefinition -> errorJson "a definition must be provided"
        Left NotInDefiningState -> errorJson "can't define right now"
        Right gs -> do
            saveGame r gs
            json gs

moveToVote :: R.Connection -> ScottyM ()
moveToVote r = post "/move-to-vote" $ do
    game <- loadGame r
    game <- return $ startVoting game
    case game of
        Left NotInDefiningState -> errorJson "can't start voting now"
        Right gs -> do
            saveGame r gs
            json gs

vote :: R.Connection -> ScottyM ()
vote r = post "/vote" $ do
    voter <- param "voter"
    votee <- param "votee"
    game <- loadGame r
    game <- return $ setPlayerVote game (Player voter) $ Player votee
    case game of
        Left NotInVotingState -> errorJson "can't vote right now"
        Right gs -> do
            saveGame r gs
            json gs

endRound :: R.Connection -> ScottyM ()
endRound r = post "/tally-votes" $ do
    game <- loadGame r
    game <- return $ endVoting game
    case game of
        Left NotInVotingState -> errorJson "can't end round right now"
        Right gs -> do
            saveGame r gs
            json gs

reset :: R.Connection -> ScottyM ()
reset r = post "/reset" $ do
    gs <- return $ WaitingForPlayers []
    saveGame r gs
    json gs

errorJson :: String -> ActionM ()
errorJson e = do
    json $ object ["error" .= e]
    status badRequest400

contentType :: LT.Text
contentType = "Content-Type"

setContentType :: LT.Text -> ActionM ()
setContentType t = setHeader contentType t

jsonUtf8 :: LT.Text
jsonUtf8 = "application/json;charset=utf-8"

htmlUtf8 :: LT.Text
htmlUtf8 = "text/html;charset=utf-8"

javascript :: LT.Text
javascript = "application/javascript"

runRedis :: R.Connection -> R.Redis a -> ActionM a
runRedis c r = liftIO $ R.runRedis c r

{-
main = do
  conn <- R.connect R.defaultConnectInfo
  scotty 3000 $ do
    get "/" $ do
      redirect $ LT.pack joinRoute
    get (literal joinRoute) $ do
      html . LT.pack $ joinTemplate []
    post (literal joinRoute) $ do
      name <- param "name"
      -- TODO: get players from a data store instead of using constants.
      if playerExists name samplePlayers then html . LT.pack $ joinTemplate [Flash "Name already taken. Please choose another."]
      else do
        setCookie (BSC.pack "name") (BSC.pack name)
        redirect $ LT.pack gameRoute
    get (literal gameRoute) $ do
      cookies <- getCookies
      case cookies of
        Just cs -> text $ cookieName cs
        Nothing -> html . LT.pack $ joinTemplate [Flash "You're not signed in. Please sign in."]
    get "/test-redis" $ do
      ping <- runRedis conn $ R.ping
      runRedis conn $ R.set "hello" "hello"
      runRedis conn $ R.set "world" "world"
      hello <- runRedis conn $ R.get "hello"
      world <- runRedis conn $ R.get "world"
      html . LT.pack $ mconcat [show ping, show hello, show world]
    get "/test-json" $ do
      json sampleGame
      setContentType jsonUtf8

data GameState = GameState { gameId :: String
                           , phase :: GamePhase
                           , players :: [Player]
                           , rounds :: [Round]
                           }
  deriving Show

data GamePhase = Starting
               | Dealing
               | Defining
               | Discussing
               | Voting
               | Over
  deriving Show

data Player = Player { name :: String }
  deriving Show

data Definition = Definition { author :: Player, definition :: String}
  deriving Show

data Vote = Vote { voter :: Player, votee :: Player }
  deriving Show

data Round = Round { roundNumber :: Int
                   , roundDealer :: Player
                   , prompt :: String
                   , definitions :: [Definition]
                   , votes :: [Vote]
                   }
  deriving Show

sampleGame :: GameState
sampleGame = GameState {gameId = "1"
                       ,phase = Starting
                       ,players = samplePlayers
                       ,rounds = [Round 1 (Player "Dude") "Mangey Susan" [Definition (Player "Dude") "A nasty rash"] [Vote (Player "Dude") (Player "Dude")]]
                       }

samplePlayers :: [Player]
samplePlayers = [Player {name = "Dude"}]

playerExists :: String -> [Player] -> Bool
playerExists n ps = any (\p -> name p == n) ps

-- Game state/data transfer encoding

instance ToJSON GameState where
  toJSON (GameState game phase players rounds) = object ["id" .= game
                                                        ,"phase" .= phase
                                                        ,"players" .= samplePlayers
                                                        ,"rounds" .= rounds
                                                        ]

instance ToJSON GamePhase where
  toJSON Starting = "starting"
  toJSON Dealing = "dealing"
  toJSON Defining = "defining"
  toJSON Discussing = "discussing"
  toJSON Voting = "voting"
  toJSON Over = "over"

instance ToJSON Definition where
  toJSON (Definition p d) = object ["player" .= p, "definition" .= d]

instance ToJSON Vote where
  toJSON (Vote s r) = object ["sender" .= s, "receiver" .= r]

instance ToJSON Player where
  toJSON (Player name) = object ["name" .= name]

instance ToJSON Round where
  toJSON (Round n d p def v) = object ["round" .= n
                                      ,"dealer" .= d
                                      ,"prompt" .= p
                                      ,"definitions" .= def
                                      ,"votes" .= v
                                      ]

-- Web entities

data Flash = Flash String
  deriving Show

joinTemplate :: [Flash] -> String
joinTemplate fs = mconcat [mconcat $ map (\(Flash m) -> wrapInDiv m) fs
                          ,"Name"
                          ,"<form method=\"POST\" action=\"join\">"
                          ,"<input type=\"text\" name=\"name\">"
                          ,"<input type=\"submit\" value=\"Join\">"
                          ,"</form>"
                          ]

wrapInDiv :: String -> String
wrapInDiv s = "<div>" ++ s ++ "</div>"

joinRoute, gameRoute :: String
joinRoute = "/join"
gameRoute = "/game"

contentType :: LT.Text
contentType = "Content-Type"

jsonUtf8 :: LT.Text
jsonUtf8 = "application/json;charset=utf-8"

setContentType :: LT.Text -> ActionM ()
setContentType t = setHeader contentType t


makeCookie :: BS.ByteString -> BS.ByteString -> SetCookie
makeCookie n v = def { setCookieName = n, setCookieValue = v }

renderSetCookie' :: SetCookie -> LT.Text
renderSetCookie' = decodeUtf8 . B.toLazyByteString . renderSetCookie

setCookie :: BS.ByteString -> BS.ByteString -> ActionM ()
setCookie n v = setHeader "Set-Cookie" (renderSetCookie' (makeCookie n v))

getCookies :: ActionM (Maybe CookiesText)
getCookies =
    fmap (fmap (parseCookiesText . lazyToStrict . encodeUtf8)) $ header "Cookie"
    where
        lazyToStrict = BS.concat . BSL.toChunks

cookiesToMap :: CookiesText -> M.Map LT.Text LT.Text
cookiesToMap cs = M.fromList $ map (\(k, v) -> (LT.fromStrict k, LT.fromStrict v)) cs

cookieName :: CookiesText -> LT.Text
cookieName cs = (cookiesToMap cs) M.! "name"

runRedis :: R.Connection -> R.Redis a -> ActionM a
runRedis c r = liftIO $ R.runRedis c r
-}