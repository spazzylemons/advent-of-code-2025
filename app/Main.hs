module Main (main) where

import System.Environment (getEnv, getArgs)
import Advent
import qualified Data.Text as T

import qualified Day1

userAgent :: AoCUserAgent
userAgent = AoCUserAgent
  { _auaRepo = T.pack "github.com/spazzylemons/advent-of-code-2025"
  , _auaEmail = T.pack "spazzylemons on github"
  }

getDay :: IO Int
getDay = do
  args <- getArgs
  arg <- case args of
    (x:_) -> pure x
    [] -> fail "Day to run not given"
  return ((read arg) :: Int)

getOpts :: IO AoCOpts
getOpts = do
  key <- getEnv "AOC_SESSION_KEY"
  return ((defaultAoCOpts userAgent 2025 key) { _aCache = Just "cache" })

getInput :: Int -> IO String
getInput day = do
  opts <- getOpts
  input <- runAoC opts $ AoCInput (mkDay_ (toInteger day))
  case input of
    Left err -> fail ("Failed to get input: " ++ (show err))
    Right result -> return (T.unpack result)

getResults :: Int -> String -> (Int, Int)
getResults day input =
  case day of
    1 -> (Day1.part1 input, Day1.part2 input)
    _ -> (0, 0)

main :: IO ()
main = do
  day <- getDay
  input <- getInput day
  let (part1, part2) = getResults day input
  putStrLn ("Part 1: " ++ (show part1))
  putStrLn ("Part 2: " ++ (show part2))
