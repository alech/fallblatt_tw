module Main where

import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Trans
import Data.Maybe
import Control.Concurrent

import Network.OAuth.Consumer
import Network.OAuth.Http.CurlHttpClient

import Network.OAuth.Http.Request
import Network.OAuth.Http.Response

import Text.JSON
import Data.Ratio

data TwitterBotConfig = TwitterBotConfig { 
		consumerKey       :: String,
		consumerSecret    :: String,
		accessToken       :: String,
		accessTokenSecret :: String
	}
	deriving (Eq, Show)

mentionsUrl params = fromJust $ parseURL $ "https://api.twitter.com/1/statuses/mentions.json" ++ params

sleepTime = 15 * 10^6 -- 15 seconds

main = do
	-- parse command line arguments
	args <- getArgs
	let parsedArgs = parseArgs args
	case parsedArgs of
		Nothing -> bailWithErrorMessage "Incorrect number of parameters, need consumer_key, consumer_secret, access_token and access_token_secret"
		_ -> return ()
	let (consumerKey, consumerSecret, accessToken, accessTokenSecret) = fromJust parsedArgs
	let twitterBotConfig = (TwitterBotConfig consumerKey consumerSecret accessToken accessTokenSecret)
	mainLoop twitterBotConfig Nothing

mainLoop :: TwitterBotConfig -> Maybe Integer -> IO ()
mainLoop twitterBotConfig lastId = do
	response <- getMentions twitterBotConfig lastId
	putStrLn $ show response
	threadDelay sleepTime
	let newLid = extractLastId response
	mainLoop twitterBotConfig (extractLastId response)

extractLastId :: String -> Maybe Integer
extractLastId json =
	maybeDecodeJSONList json >>= listToMaybe >>= extractEntry "id" >>= extractNumFromEntry
	where
		extractNumFromEntry (JSRational False r) = Just $ numerator r
		extractNumFromEntry _                    = Nothing

maybeDecodeJSONList :: String -> Maybe [JSObject JSValue]
maybeDecodeJSONList = okToMaybe . decode

okToMaybe :: Result a -> Maybe a
okToMaybe (Ok j) = Just j
okToMaybe _      = Nothing

-- TODO: is there a way to write this point-free?
extractEntry :: String -> JSObject JSValue -> Maybe JSValue
extractEntry key obj = okToMaybe $ valFromObj key obj

parseArgs :: [(String)] -> Maybe (String, String, String, String)
parseArgs args =
	if length args /= 4 then
		Nothing
	else
		Just (args !! 0, args !! 1, args !! 2, args !! 3)

bailWithErrorMessage :: String -> IO ()
bailWithErrorMessage msg = do 
	hPutStrLn stderr msg
	exitFailure

twitterToken :: String -> String -> [(String, String)]
twitterToken accessToken accessTokenSecret = [("oauth_token", accessToken),
                ("oauth_token_secret", accessTokenSecret)]

loadToken :: TwitterBotConfig -> OAuthMonadT IO ()
loadToken (TwitterBotConfig consumerKey consumerSecret accessToken accessTokenSecret) =
	(return (AccessToken (Application consumerKey consumerSecret OOB) (fromList $ twitterToken accessToken accessTokenSecret))) >>= putToken

tokenFromConfig :: TwitterBotConfig -> Token
tokenFromConfig (TwitterBotConfig consumerKey consumerSecret _ _) =
	fromApplication $ Application consumerKey consumerSecret OOB

getMentions :: TwitterBotConfig -> Maybe Integer -> IO String
getMentions config lastId = do
	let token = tokenFromConfig config
	let sinceId = case lastId of
		Nothing -> ""
		Just id -> "?since_id=" ++ (show id)
	-- TODO handle network-level failure by using runOAuth instead of
	-- runOAuthM with corresponding fail function (need to learn more about
	-- lifting to lift L.unpack and rspPayload ...)
	response <- runOAuthM token $ do
		loadToken config
		signRq2 HMACSHA1 Nothing (mentionsUrl sinceId) >>= serviceRequest CurlClient
	return $ L.unpack $ rspPayload response

