import Data.List.Split
import Data.Typeable
import GHC.IO.Exception
import System.Directory
import System.Exit
import System.IO
import System.IO.Error
import System.Process

main = do
  hSetBuffering stdout NoBuffering
  loop

loop = do
  putStr "> "
  line <- getLine

  let splitLine = splitOn " " line
  let cleanLine = filter (\x -> x /= "") splitLine

  exitCode <- runCmd cleanLine
  if exitCode /= ExitSuccess
    then putStrLn ("exit code " ++ (show exitCode)) else return ()

  loop

ioRunCatch :: IO () -> IO ()
ioRunCatch f = catchIOError f handler
  where
    handler :: IOError -> IO ()
    handler e = do
      case e of
        _ | isDoesNotExistError e -> putStrLn "error: file or directory does not exist"
        --_ | InappropriateType e -> putStrLn "error: unexpected type"
        _ -> putStrLn ("error: unhandled io exception " ++ (show e) ++ " " ++ (show (typeOf e)))

runCmd :: [String] -> IO ExitCode
runCmd [] = do
  return ExitSuccess
runCmd ("cd" : []) = do
  path <- getHomeDirectory
  ioRunCatch (setCurrentDirectory path)
  return ExitSuccess
runCmd ("cd" : args) = do
  ioRunCatch (setCurrentDirectory (head args))
  return ExitSuccess
runCmd ("about" : _) = do
  putStrLn "hsell shell"
  return ExitSuccess
runCmd ("exit" : _) = do
  exitSuccess
  return ExitSuccess
runCmd (cmd : args) = do
  (_, _, _, ph) <- createProcess (proc cmd args)
  waitForProcess ph
