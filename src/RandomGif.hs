{-# LANGUAGE OverloadedStrings #-}
module RandomGif where
import Data.Aeson	
import Data.Text as T (Text, pack, unpack)
import Data.ByteString.Lazy.Internal
import qualified Data.HashMap.Strict as Map
import Data.Vector (fromList, toList)
import Network.HTTP
import Data.Typeable
import Data.Maybe
import Data.List
import Network.Stream
import Debug.Trace
import System.Random

type GifFailure = String

baseUrl :: [String] -> String
baseUrl query = "http://api.giphy.com/v1/gifs/search?q="++(concat $ intersperse "+" query)++"&api_key=dc6zaTOxFJmzC&limit=10"

queryTerms = ["dogs", "programming", "pirate", "funny", "technology", "epic", "win", "fail", "computer"]

randomGifUrl :: IO (Either GifFailure String)
randomGifUrl = do
	query <- randomQuery
	queryResult <- performRequest query
	case queryResult of 
		Nothing -> return $ Left "GIF could not be retrieved"
		(Just validGifUrl) -> return $ Right validGifUrl

randomQuery :: IO String
randomQuery = do
	randomIndex1 <- randomIO
	randomIndex2 <- randomIO
	return $ baseUrl $ chooseTerms randomIndex1 randomIndex2
	where 

chooseTerms :: Int -> Int -> [String]
chooseTerms i1 i2 
	| i1' /= i2' =  map (queryTerms !!) [i1', i2']
	| otherwise =  [queryTerms!!i1]
	where 
		max = length queryTerms
		i1' = i1 `mod` max
		i2' = i2 `mod` max

performRequest :: String -> IO (Maybe String)
performRequest query = 
	do 
		response <- simpleHTTP $ getRequest (baseUrl ["dogs"])
		return $ convertResponse response

convertResponse :: Either ConnError (Response String) -> Maybe String
convertResponse (Right response) = case gifUrls (rspBody response) of 
	[] -> Nothing
	urls -> Just (urls!!0) --TODO pick one of them urls
convertResponse _ = Nothing

gifUrls :: String -> [String]
gifUrls body = case (gifUrlsFromValue (decode (packChars body) :: Maybe Value)) of
	Just urls -> urls
	Nothing -> []

gifUrlsFromValue :: Maybe Value -> Maybe [String]
gifUrlsFromValue (Just (Object obj)) = 
	case Map.lookup (T.pack "data") obj of 
		(Just val) -> gifUrlsFromValue' val
		Nothing -> Nothing
		-- something else??

gifUrlsFromValue _ = Nothing


gifUrlsFromValue' :: Value -> Maybe [String]
gifUrlsFromValue' (Array values) = Just $ catMaybes $ map (pluckUrl ["images", "original", "url"]) (toList values)

pluckUrl :: [String] -> Value  -> Maybe String
pluckUrl [] (String originalUrl) = Just  $ T.unpack originalUrl
pluckUrl (crumb:crumbs) (Object value) = 
	case Map.lookup (T.pack crumb) value of
		Just (url) -> pluckUrl crumbs (url)
		_ -> Nothing

pluckUrl _ _ = Nothing