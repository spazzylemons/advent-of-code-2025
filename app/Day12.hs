{-# LANGUAGE OverloadedStrings #-}
module Day12 (Day12Puzzle(..)) where
import qualified Data.Text as T
import Util

data Region = Region Int [Int]
data Day12Puzzle = Day12Puzzle [Int] [Region]
data ParseState = ParseShapes Int | ParseRegions

parseInput' :: ParseState -> [T.Text] -> Day12Puzzle -> Maybe Day12Puzzle
parseInput' ParseRegions [] puzzle = Just puzzle
parseInput' ParseRegions (x:xs) (Day12Puzzle shapes regions) = do
  (size:nums:[]) <- Just (T.splitOn ": " x)
  (width:height:[]) <- Just ((read . T.unpack) <$> (T.splitOn "x" size))
  let amounts = (read . T.unpack) <$> (T.words nums)
  parseInput' ParseRegions xs (Day12Puzzle shapes ((Region (width * height) amounts):regions))
parseInput' (ParseShapes 6) text (Day12Puzzle shapes regions) = parseInput' ParseRegions text (Day12Puzzle (reverse shapes) regions)
parseInput' (ParseShapes count) (_:a:b:c:_:xs) (Day12Puzzle shapes regions) =
  parseInput' (ParseShapes (succ count)) xs (Day12Puzzle (length (filter (=='#') ((mconcat (T.unpack <$> [a, b, c])))):shapes) regions)
parseInput' _ _ _ = Nothing

instance Puzzle Day12Puzzle where
  parseInput input =
    parseInput' (ParseShapes 0) (T.lines input) (Day12Puzzle [] [])

  part1 (Day12Puzzle shapes regions) =
    length $ filter (\(Region size amounts) -> (sum (uncurry (*) <$> zip shapes amounts)) <= size) regions

  part2 = const 0
