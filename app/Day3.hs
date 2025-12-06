module Day3 (Day3Puzzle(..)) where
import qualified Data.Text as T
import Util

type Bank = [Int]
type Input = [Bank]

newtype Day3Puzzle = Day3Puzzle Input

parseBank :: T.Text -> Bank
parseBank line =
  (read . pure) <$> (T.unpack line)

testDigits :: Int -> Int -> Int -> Bank -> Int
testDigits remaining acc digit xs =
  case testDigit remaining acc digit xs of
    0 -> case digit of
      1 -> 0
      _ -> testDigits remaining acc (pred digit) xs
    result -> result

testDigit :: Int -> Int -> Int -> Bank -> Int
testDigit _ _ _ [] = 0
testDigit remaining acc digit (x:xs) =
  let
    newAcc = acc * 10 + digit
    isMatch = x == digit
  in case (remaining, isMatch) of
    (_, False) -> testDigit remaining acc digit xs
    (1, True) -> newAcc
    (_, True) -> testDigits (pred remaining) newAcc 9 xs

testDigits0 :: Int -> Bank -> Int
testDigits0 count =
  testDigits count 0 9

instance Puzzle Day3Puzzle where
  parseInput input = do
    return (Day3Puzzle (parseBank <$> (T.lines input)))

  part1 (Day3Puzzle banks) =
    sum (map (testDigits0 2) banks)

  part2 (Day3Puzzle banks) =
    sum (map (testDigits0 12) banks)
