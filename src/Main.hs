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

program :: [String] -> IO ()
program [_, username, repository] = runBot username repository 
program (progName:_) = putStrLn $ "Usage: " ++ progName ++ " USERNAME REPONAME" 

runBot :: Username -> RepoName -> IO ()
runBot username repoName = 
	do 
		botUser <- (BS.getLine)
		password <- (BS.getLine)
		processRepo (Auth.GithubBasicAuth (BS.init botUser) (BS.init password)) username repoName

processRepo :: Auth.GithubAuth -> Username -> RepoName -> IO ()
processRepo gitUser username repoName = do 
		pullRequests <- (Pulls.pullRequestsFor username repoName) 
		commentLists <- comments username repoName pullRequests
		doCommentPulls username repoName gitUser (pullsMissingGIF commentLists)

doCommentPulls :: String -> String -> Auth.GithubAuth -> [GitData.Issue] -> IO ()
doCommentPulls username repoName auth comments = do
	putStrLn $ unwords ["Got", (show $ length comments), "pull requests that are missing GIFs"]
	commentOnPulls auth username repoName comments

commentOnPulls :: Auth.GithubAuth -> Username -> RepoName -> [GitData.Issue] -> IO ()
commentOnPulls _ _ _ [] = putStrLn "Done commenting"
commentOnPulls auth username repoName (issue:issues) = 
	do 
		IssueComments.createComment auth username repoName (GitData.issueNumber issue) "No GIF?"
		commentOnPulls auth username repoName issues

pullsMissingGIF :: [GitData.Issue] -> [GitData.Issue]
pullsMissingGIF = filter (not.hasAppropriateGIF)

comments :: Username -> RepoName -> Either GitData.Error [GitData.PullRequest]  -> IO [GitData.Issue]
comments _ _ (Left _) = putStrLn "Error occurred while processing pull request" >> return []
comments user repo (Right pullRequests) =  
	do
		--- uhhhhh.... 
		convertIOMaybe maybeIssues
		where 
			pullIds =  map (pluckPullRequestId.GitData.pullRequestIssueUrl) pullRequests
			maybeIssues = map (commentsFromIssueUrl user repo) pullIds

convertIOMaybe :: [IO (Maybe a)] -> IO [a]
convertIOMaybe [] = return []
convertIOMaybe (x:xs) = 
	do 
		x' <- x
		xs' <- convertIOMaybe xs
		return ((maybeToList x') ++ (xs'))

commentsFromIssueUrl :: Username -> RepoName -> Int -> IO (Maybe GitData.Issue)
commentsFromIssueUrl user repo pullId = 
	do 
		issue <- Issues.issue user repo pullId 
		return $ toList issue

toList :: Either a b -> Maybe b
toList (Left _) = Nothing
toList (Right x) = Just x

hasAppropriateGIF ::  GitData.Issue  -> Bool
hasAppropriateGIF issue = (body Re.=~ regexString) :: Bool
	where 
		regexString = "!\\[.+\\]\\(.+\\)"
		body = if (GitData.issueBody issue) == Nothing then "" else fromJust (GitData.issueBody issue)

pluckPullRequestId :: String -> Int
pluckPullRequestId str = parseId numstr
	where numstr = reverse $ pluck (reverse str)

parseId :: String -> Int
parseId i = fst $ (reads i :: [(Int, String)])!!0

pluck :: String -> String
pluck ('/':_) = ""
pluck (s:ss) = s:(pluck ss)