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
import qualified Network.OAuth.Http.PercentEncoding as PE

import Text.JSON
import Data.Ratio
import Text.Printf
import Network.Socket
import Data.Char (toLower)

data TwitterBotConfig = TwitterBotConfig { 
		consumerKey       :: String,
		consumerSecret    :: String,
		accessToken       :: String,
		accessTokenSecret :: String
	}
	deriving (Eq, Show)

mentionsUrl params = fromJust $ parseURL $ "https://api.twitter.com/1/statuses/mentions.json" ++ params
tweetUrl = fromJust $ parseURL "https://api.twitter.com/1/statuses/update.json"

sleepTime     = 15 * 10^6 -- 15 seconds
maxSize       = 15 -- Fallblattanzeige has 15 chars
fallblattHost = "127.0.0.1"
fallblattPort = 1337

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
	mentionsResponse <- getMentions twitterBotConfig lastId
	let currentId  = extractLastId mentionsResponse
	let mention    = extractMention mentionsResponse
	let screenName = extractScreenName mentionsResponse
	putStrLn $ "lastId: " ++ (show lastId)
	putStrLn $ "currentId: " ++ (show currentId)
	putStrLn $ "mention: " ++ (show mention)
	putStrLn $ "screenName: " ++ (show screenName)
	fallblattResponse <- maybeShowOnFallblatt mention screenName
	putStrLn $ "fallblattresponse: " ++ (show fallblattResponse)
	maybeTweetReply lastId twitterBotConfig screenName currentId fallblattResponse
	threadDelay sleepTime
	mainLoop twitterBotConfig (nothingOr currentId lastId)

maybeShowOnFallblatt :: Maybe String -> Maybe String -> IO (Maybe String)
maybeShowOnFallblatt (Just mention) maybeScreenName = do
	if length mention > maxSize then
		return (Just $ "TMI. Please limit yourself to " ++ (show maxSize) ++ " characters!")
	else
		setFallblatt $ maybeCombine mention maybeScreenName
maybeShowOnFallblatt _ _ = do
	return Nothing

-- if there is enough space, combine screen name and mention, i.e.
-- "alech: foobar"
maybeCombine :: String -> Maybe String -> String
maybeCombine mention (Just screenName) =
	head $ filter (\e -> length e <= maxSize) [combined, mention]
	where combined = screenName ++ ": " ++ mention
maybeCombine mention Nothing = mention

setFallblatt :: String -> IO (Maybe String)
setFallblatt text = withSocketsDo $ do
	s <- socket AF_INET Datagram defaultProtocol
	hostAddr <- inet_addr fallblattHost
	sendTo s (printf ("%-" ++ (show maxSize) ++ "s\n") text) (SockAddrInet fallblattPort hostAddr)
	response <- recv s 16
	sClose s
	return (Just response)

maybeTweetReply :: (Maybe Integer) -> TwitterBotConfig -> (Maybe String) -> (Maybe Integer) -> (Maybe String) -> IO ()
-- tweet only if we already have a last ID, i.e. not in the first iteration
maybeTweetReply (Just _) config (Just screenName) (Just tweetId) (Just text) = do
	let replyText = "@" ++ screenName ++ " " ++ text
	let token = tokenFromConfig config
	response <- runOAuthM token $ do
		loadToken config
		signRq2 HMACSHA1 Nothing tweetUrl { method = POST,
		                                    reqHeaders = fromList [("Content-Type", "application/x-www-form-urlencoded")],
                                            reqPayload = urlEncode [("status", replyText), ("in_reply_to_status_id", show tweetId)]
                                          } >>= serviceRequest CurlClient
	putStrLn $ "Tweet reply response: " ++ (show response)
	return ()
maybeTweetReply _ _ _ _ _ = do
	return ()

urlEncode :: [(String,String)] -> L.ByteString
urlEncode = L.pack . init . concatMap (\(k,v) -> PE.encode k ++ "=" ++ PE.encode v ++ "&")

extractLastId :: String -> Maybe Integer
extractLastId json =
	maybeDecodeJSONList json >>= listToMaybe >>= (extractEntry "id" :: JSObject JSValue -> Maybe JSValue) >>= extractNumFromEntry
	where
		extractNumFromEntry (JSRational False r) = Just $ numerator r
		extractNumFromEntry _                    = Nothing

extractMention :: String -> Maybe String
extractMention json =
	maybeDecodeJSONList json >>= listToMaybe >>= (extractEntry "text" :: JSObject JSValue -> Maybe JSString) >>= Just . unwords . filter notAtFallblatt . words . fromJSString
	where notAtFallblatt word = "@fallblatt" /= map toLower word -- FIXME point-free?

-- nested JSON
extractScreenName :: String -> Maybe String
extractScreenName json =
	maybeDecodeJSONList json >>= listToMaybe >>= (extractEntry "user" :: JSObject JSValue -> Maybe (JSObject JSValue)) >>= (extractEntry "screen_name" :: JSObject JSValue -> Maybe JSString) >>= Just . fromJSString

maybeDecodeJSONList :: String -> Maybe [JSObject JSValue]
maybeDecodeJSONList = resultToMaybe . decode

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Ok j) = Just j
resultToMaybe _      = Nothing

-- TODO: is there a way to write this point-free?
extractEntry key obj = resultToMaybe $ valFromObj key obj

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

nothingOr :: Maybe a -> Maybe a -> Maybe a
nothingOr (Just a) _        = Just a
nothingOr Nothing  (Just b) = Just b
nothingOr Nothing Nothing   = Nothing

