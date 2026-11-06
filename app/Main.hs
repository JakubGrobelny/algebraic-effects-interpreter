module Main (main) where

import qualified Data.Text.IO as TIO
import qualified Lang.Parser as Parser
import Control.Monad (forM_)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, stderr)

data Flag = Help
  deriving (Eq)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"] (NoArg Help) "show this help message"
  ]

usage :: String -> String
usage progName = usageInfo ("Usage: " ++ progName ++ " [OPTION...] FILE") options

parseArgs :: IO [FilePath]
parseArgs = do
  args <- getArgs
  progName <- getProgName
  let failWith errs = do
        hPutStr stderr $ concat errs ++ usage progName
        exitFailure
  case getOpt Permute options args of
    (flags, _, []) | Help `elem` flags -> do
      putStr $ usage progName
      exitSuccess
    (_, [], []) -> failWith ["missing input file\n"]
    (_, files, []) -> return files
    (_, _, errs) -> failWith errs

main :: IO ()
main = do
  filePaths <- parseArgs
  forM_ filePaths $ \filePath -> do
    source <- TIO.readFile filePath
    putStrLn $ filePath ++ ":"
    putStrLn . show $ Parser.parseProgram filePath source
    return ()
