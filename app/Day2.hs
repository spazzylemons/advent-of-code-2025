{-# LANGUAGE OverloadedStrings #-}
module Day2 (Day2Puzzle(..)) where
import qualified Data.Text as T
import Util

type IdRange = (Int, Int)
type Input = [IdRange]

newtype Day2Puzzle = Day2Puzzle Input

toDigits :: Int -> [Int]
toDigits x
  | 0 <= x && x <= 9 = [x]
  | otherwise =
    let (remaining, digit) = x `divMod` 10 in
      digit:(toDigits remaining)

partition' :: [a] -> Int -> [[a]]
partition' [] _ = []
partition' xs size =
  start:(partition' rest size)
  where
    (start, rest) = splitAt size xs

partition :: Int -> [a] -> Maybe [[a]]
partition n xs
  | m * n == l = Just (partition' xs m)
  | otherwise = Nothing
  where
    l = length xs
    m = l `div` n

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (_:[]) = True
allSame (a:b:xs)
  | a == b = allSame (b:xs)
  | otherwise = False

isInvalid :: [Int] -> Int -> Bool
isInvalid digits partitions =
  case (partition partitions digits) of
    Just parts -> allSame parts
    Nothing -> False

isInvalidIdP1 :: Int -> Bool
isInvalidIdP1 productId = isInvalid (toDigits productId) 2

isInvalidIdP2 :: Int -> Bool
isInvalidIdP2 productId =
  any (isInvalid digits) [2..length digits]
  where digits = toDigits productId

sumInvalidIds :: (Int -> Bool) -> Int -> IdRange -> Int
sumInvalidIds test acc (a, b) =
  acc + (sum $ filter test [a..b])

parseRange :: T.Text -> Maybe IdRange
parseRange line = do
  (a:b:[]) <- pure (T.splitOn "-" line)
  return (read $ T.unpack a, read $ T.unpack b)

instance Puzzle Day2Puzzle where
  parseInput input = do
    ranges <- sequence $ parseRange <$> T.splitOn "," input
    return (Day2Puzzle ranges)

  part1 (Day2Puzzle ranges) =
    foldl (sumInvalidIds isInvalidIdP1) 0 ranges

  part2 (Day2Puzzle ranges) =
    foldl (sumInvalidIds isInvalidIdP2) 0 ranges
