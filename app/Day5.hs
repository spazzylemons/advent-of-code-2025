{-# LANGUAGE OverloadedStrings #-}
module Day5 (Day5Puzzle(..)) where
import qualified Data.List as L
import qualified Data.Text as T
import Util

type Range = (Int, Int)

data Day5Puzzle = Day5Puzzle
  { ranges :: [Range]
  , ingredients :: [Int]
  }

parseRange :: T.Text -> Maybe Range
parseRange line = do
  (a:b:[]) <- pure (T.splitOn "-" line)
  return (read $ T.unpack a, read $ T.unpack b)

parseRanges :: T.Text -> Maybe [Range]
parseRanges input =
  sequence $ parseRange <$> T.lines input

parseIngredients :: T.Text -> [Int]
parseIngredients input =
  read . T.unpack <$> T.lines input

part2Iter :: Int -> [Range] -> Int
part2Iter acc [] = acc
part2Iter acc ((start, end):xs) =
  part2Iter newAcc newRanges
  where
    newAcc = acc + succ (end - start)
    newRanges = filter isValidRange $ limitRange <$> xs
    isValidRange (s, e) = s <= e
    limitRange (s, e) = (succ end `max` s, e)

instance Puzzle Day5Puzzle where
  parseInput input = do
    (rangesInput:ingredientsInput:[]) <- pure (T.splitOn "\n\n" input)
    ranges <- parseRanges rangesInput
    let ingredients = parseIngredients ingredientsInput
    return (Day5Puzzle { ranges, ingredients })

  part1 (Day5Puzzle { ranges, ingredients }) =
    length $ filter contains ingredients
    where
      contains num =
        any inRange ranges
        where
          inRange (start, end) = num >= start && num <= end

  part2 (Day5Puzzle { ranges }) =
    part2Iter 0 $ L.sort ranges
