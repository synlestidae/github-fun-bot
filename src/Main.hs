{-# LANGUAGE OverloadedStrings #-}
import qualified Github.Repos as Repos
import qualified Github.PullRequests as Pulls
import qualified Github.Issues as Issues
import qualified Github.Issues.Comments as IssueComments
import qualified Github.Data.Definitions as GitData
import qualified Github.Auth as Auth
import qualified Data.ByteString as BS
import System.Environment
import RandomGif as Gif
import Control.Monad
import Control.Applicative
import Data.Maybe
import Text.Regex.Posix as Re
import Debug.Trace

main :: IO ()
main = do
	args <- getArgs
	progName <- getProgName
	program (progName:args)

type Username = [Char]
type Password = Username
type RepoName = Password

botUser = "****"
botPassword = "****"

program :: [String] -> IO ()
program [_, username, repository] = runBot username repository
program (progName:_) = putStrLn $ "Usage: " ++ progName ++ " USERNAME REPONAME" 

runBot :: Username -> RepoName -> IO ()
runBot username repoName = 
	do 
		--botUser <- (BS.getLine)
		--password <- (BS.getLine)
		processRepo (Auth.GithubBasicAuth botUser (botPassword)) username repoName

processRepo :: Auth.GithubAuth -> Username -> RepoName -> IO ()
processRepo gitUser username repoName = do 
		pullRequests <- (Pulls.pullRequestsFor' (Just gitUser) username repoName) 
		commentLists <- comments (Just gitUser) username repoName pullRequests
		doCommentPulls (Just gitUser) username repoName gitUser (pullsMissingGIF commentLists) 

doCommentPulls ::  Maybe Auth.GithubAuth -> String -> String -> Auth.GithubAuth -> [GitData.Issue] -> IO ()
doCommentPulls gitUser username repoName auth comments = do-- have to filter those ones with gifs in later comments
	putStrLn $ unwords ["Got", (show $ length comments), "pull requests that don't have GIFs in the description"]
	commentOnPulls auth username repoName comments

commentOnPulls :: Auth.GithubAuth -> Username -> RepoName -> [GitData.Issue] -> IO ()
commentOnPulls _ _ _ [] = putStrLn "Done commenting"
commentOnPulls auth username repoName (issue:issues) = 
	do 
		gifUrl <- randomGifUrl
		--hasGifs <- commentsWithGifs (Just auth) username repoName issue
		fileContents <- readFile "COMMENTED.txt"
		case gifUrl of 
			Left failureMessage -> putStrLn "Could not find a GIF for the comment"
			Right url -> if not $ (show $ GitData.issueNumber issue) `elem` lines fileContents then do
							writeFile "COMMENTED.txt" (unlines ((show $ GitData.issueNumber issue) : (lines fileContents)))
							(IssueComments.createComment auth username repoName (GitData.issueNumber issue) (makeComment url)) 
							putStrLn "Commented ^_^"
						 else putStrLn "Looks like there is an existing GIF"
		commentOnPulls auth username repoName issues

makeComment :: String -> String
makeComment url = concat $ ["Found you a nice GIF for your awesome pull requests	.\n", "![Alt text](",url,")"]

pullsMissingGIF :: [GitData.Issue] -> [GitData.Issue]
pullsMissingGIF = filter (not.hasAppropriateGIF)

comments :: Maybe Auth.GithubAuth -> Username -> RepoName -> Either GitData.Error [GitData.PullRequest]  -> IO [GitData.Issue]
comments _ _ _ (Left err) = putStrLn ("Error occurred while processing pull request:"++(show err)) >> return []
comments gitUser user repo (Right pullRequests) =  
	do
		--- uhhhhh.... 
		convertIOMaybe maybeIssues
		where 
			pullIds =  map (pluckPullRequestId.GitData.pullRequestIssueUrl) pullRequests
			maybeIssues = map (commentsFromIssueUrl gitUser user repo) pullIds

convertIOMaybe :: [IO (Maybe a)] -> IO [a]
convertIOMaybe [] = return []
convertIOMaybe (x:xs) = 
	do 
		x' <- x
		xs' <- convertIOMaybe xs
		return ((maybeToList x') ++ (xs'))

commentsFromIssueUrl :: Maybe Auth.GithubAuth -> Username -> RepoName -> Int -> IO (Maybe GitData.Issue)
commentsFromIssueUrl auth user repo pullId = 
	do 
		issue <- Issues.issue' auth user repo pullId 
		return $ toList issue

commentsWithGifs :: Maybe Auth.GithubAuth -> Username -> RepoName -> GitData.Issue -> IO Bool
commentsWithGifs auth user repoName (issue) = do
	result <- IssueComments.comments' auth user repoName (GitData.issueId issue)
	case result of 
		Left err -> putStrLn ("Error while processing comments: "++show err)>> return True
		Right theComments -> return $ any (\b -> trace (GitData.issueCommentBody b) (GitData.issueCommentBody b) Re.=~ regexString) theComments

toList :: Either a b -> Maybe b
toList (Left _) = Nothing
toList (Right x) = Just x

hasAppropriateGIF ::  GitData.Issue  -> Bool
hasAppropriateGIF issue = trace ("Body:"++body) (body Re.=~ regexString) :: Bool
	where 
		body = if (GitData.issueBody issue) == Nothing then "" else fromJust (GitData.issueBody issue)

regexString :: String
regexString = "!\\[.+\\]\\(.+\\)"

pluckPullRequestId :: String -> Int
pluckPullRequestId str = parseId numstr
	where numstr = reverse $ pluck (reverse str)

parseId :: String -> Int
parseId i = fst $ (reads i :: [(Int, String)])!!0

pluck :: String -> String
pluck ('/':_) = ""
pluck (s:ss) = s:(pluck ss)