import qualified Github.Repos as Repos
import qualified Github.PullRequests as Pulls
import qualified Github.Data.Definitions as GitData
import System.Environment
import Control.Applicative

main :: IO ()
main = do
	args <- getArgs
	progName <- getProgName
	program (progName:args)

type Username = [Char]
type Password = Username
type RepoName = Password

program :: [String] -> IO ()
program [_, username, password, repository] = runBot (username, password) repository
program (progName:_) = putStrLn $ "Usage: " ++ progName ++ " USERNMAE PASSWORD REPONAME" 

runBot :: (Username, Password) -> RepoName -> IO ()
runBot (username, _) repoName = processRepo username repoName

processRepo :: Username -> RepoName -> IO ()
processRepo username repoName = do 
		pullRequests <- (Pulls.pullRequestsFor username repoName) 
		if (hasAppropriateGif pullRequests) then putStrLn "This pull request has a GIF." else putStrLn "This pull request needs a GIF folks!"

hasAppropriateGif ::  Either GitData.Error [GitData.PullRequest]  -> Bool
hasAppropriateGif (Right pullRequest) = any (anyCommentsHaveGif) pullRequest
hasAppropriateGif (Left _) = True

anyCommentsHaveGif :: GitData.PullRequest -> Bool
anyCommentsHaveGif pullRequest = True