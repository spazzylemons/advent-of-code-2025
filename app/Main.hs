{-# LANGUAGE AllowAmbiguousTypes #-}
module Main (main) where

import System.Environment (getEnv, getArgs)
import System.Exit (exitFailure)
import Advent
import qualified Data.Text as T

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
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

runPuzzle :: Puzzle a => a -> IO ()
runPuzzle puzzle = do
  putStrLn ("Part 1: " ++ (show . part1 $ puzzle))
  putStrLn ("Part 2: " ++ (show . part2 $ puzzle))

parsePuzzle :: Puzzle a => T.Text -> IO a
parsePuzzle text =
  case parseInput text of
    Just puzzle -> return puzzle
    Nothing -> do
      putStrLn "Failed to parse input"
      exitFailure

main :: IO ()
main = do
  day <- getDay
  input <- getInput day
  case day of
    1 -> do
      p :: Day1Puzzle <- parsePuzzle input
      runPuzzle p
    2 -> do
      p :: Day2Puzzle <- parsePuzzle input
      runPuzzle p
    3 -> do
      p :: Day3Puzzle <- parsePuzzle input
      runPuzzle p
    4 -> do
      p :: Day4Puzzle <- parsePuzzle input
      runPuzzle p
    5 -> do
      p :: Day5Puzzle <- parsePuzzle input
      runPuzzle p
    6 -> do
      p :: Day6Puzzle <- parsePuzzle input
      runPuzzle p
    7 -> do
      p :: Day7Puzzle <- parsePuzzle input
      runPuzzle p
    8 -> do
      p :: Day8Puzzle <- parsePuzzle input
      runPuzzle p
    9 -> do
      p :: Day9Puzzle <- parsePuzzle input
      runPuzzle p
    10 -> do
      p :: Day10Puzzle <- parsePuzzle input
      runPuzzle p
    _ -> do
      putStrLn ("Day " ++ (show day) ++ " not implemented")
      exitFailure
