import Github.Repos
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	progName <- getProgName
	program (progName:args)

type Username = [Char]
type Password = [Char]
type RepoName = [Char]

program :: [String] -> IO ()
program [_, username, password, repository] = runBot (username, password) repository
program (progName:_) = putStrLn $ "Usage: " ++ progName ++ " USERNAME PASSWORD REPONAME" 

runBot :: (Username, Password) -> RepoName -> IO ()
runBot _ _ = putStrLn "Duhhhh... I need to do stuff"
