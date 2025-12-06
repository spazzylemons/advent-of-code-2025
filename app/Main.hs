module Main (main) where

import System.Environment (getEnv, getArgs)
import Advent
import qualified Data.Text as T

import Day1
import Day2
import Day3
import Day4
import Day5
import Util

userAgent :: AoCUserAgent
userAgent = AoCUserAgent
  { _auaRepo = T.pack "github.com/spazzylemons/advent-of-code-2025"
  , _auaEmail = T.pack "spazzylemons on github"
  }

getDay :: IO Int
getDay = do
  (arg:_) <- getArgs
  return ((read arg) :: Int)

getOpts :: IO AoCOpts
getOpts = do
  key <- getEnv "AOC_SESSION_KEY"
  return ((defaultAoCOpts userAgent 2025 key) { _aCache = Just "cache" })

getInput :: Int -> IO T.Text
getInput day = do
  opts <- getOpts
  Right input <- runAoC opts $ AoCInput (mkDay_ (toInteger day))
  return input

runPuzzle :: Puzzle a => (T.Text -> Maybe a) -> T.Text -> IO ()
runPuzzle parser input = do
  Just puzzle <- pure (parser input)
  putStrLn ("Part 1: " ++ (show . part1 $ puzzle))
  putStrLn ("Part 2: " ++ (show . part2 $ puzzle))

main :: IO ()
main = do
  day <- getDay
  input <- getInput day
  case day of
    1 -> (runPuzzle (parseInput :: (T.Text -> Maybe Day1Puzzle)) input)
    2 -> (runPuzzle (parseInput :: (T.Text -> Maybe Day2Puzzle)) input)
    3 -> (runPuzzle (parseInput :: (T.Text -> Maybe Day3Puzzle)) input)
    4 -> (runPuzzle (parseInput :: (T.Text -> Maybe Day4Puzzle)) input)
    5 -> (runPuzzle (parseInput :: (T.Text -> Maybe Day5Puzzle)) input)
    _ -> putStrLn ("Day " ++ (show day) ++ " not supported")
