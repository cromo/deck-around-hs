{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

import Data.Monoid (mconcat)
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding (encodeUtf8,decodeUtf8)
import Data.Aeson (ToJSON(toJSON),object,(.=))
import Web.Cookie
import qualified Database.Redis as R

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Map as M

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