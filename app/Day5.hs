{-# LANGUAGE OverloadedStrings #-}
module Day5 (part1, part2) where
import qualified Data.List as L
import qualified Data.Text as T

type Range = (Int, Int)
data Input = Input
  { ranges :: [Range]
  , ingredients :: [Int]
  }

parseRange :: T.Text -> Range
parseRange input =
  case T.splitOn "-" input of
    a:b:[] -> (read $ T.unpack a, read $ T.unpack b)
    _ -> error "Invalid input"

parseRanges :: T.Text -> [Range]
parseRanges input =
  parseRange <$> T.lines input

parseIngredients :: T.Text -> [Int]
parseIngredients input =
  read . T.unpack <$> T.lines input

parseInput :: T.Text -> Input
parseInput input =
  case T.splitOn "\n\n" input of
    (rangesInput:ingredientsInput:[]) ->
      let
        ranges = parseRanges rangesInput
        ingredients = parseIngredients ingredientsInput
      in
        Input { ranges, ingredients }
    _ -> error "Invalid input"

part2Iter :: Int -> [Range] -> Int
part2Iter acc [] = acc
part2Iter acc ((start, end):xs) =
  part2Iter newAcc newRanges
  where
    newAcc = acc + succ (end - start)
    newRanges = filter isValidRange $ limitRange <$> xs
    isValidRange (s, e) = s <= e
    limitRange (s, e) = ((succ end) `max` s, e)

part1 :: T.Text -> Int
part1 input =
  length (filter contains ingredients)
  where
    Input { ranges, ingredients } = parseInput input
    contains num =
      any inRange ranges
      where
        inRange (start, end) = num >= start && num <= end

part2 :: T.Text -> Int
part2 input =
  part2Iter 0 (L.sort ranges)
  where
    Input { ranges } = parseInput input
