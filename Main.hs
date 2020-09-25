import Data.List.Split
import System.Directory
import System.IO
import System.Process

main = do
  putStr "> "
  line <- getLine

  let splitLine = splitOn " " line
  let cleanLine = filter (\x -> x /= "") splitLine

  _ <- runCmd cleanLine

  main

runCmd :: [String] -> IO ()
runCmd [] = do
  return ()
runCmd ("cd" : []) = do
  path <- getHomeDirectory
  setCurrentDirectory path
runCmd ("cd" : args) = do
  setCurrentDirectory (head args)
runCmd ("about" : _) = do
  putStrLn "hsell shell"
runCmd (cmd : args) = do
  (_, _, _, ph) <- createProcess (proc cmd args)
  waitForProcess ph
  return ()
