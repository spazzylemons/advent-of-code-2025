module Day3 (part1, part2) where
import qualified Data.Text as T

type Bank = [Int]
type Input = [Bank]

parseBank :: T.Text -> Bank
parseBank line =
  map (read . pure) $ T.unpack line

parseInput :: T.Text -> Input
parseInput input =
  map parseBank $ T.lines input

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

part1 :: T.Text -> Int
part1 input =
  sum (map (testDigits0 2) banks)
  where
    banks = parseInput input

part2 :: T.Text -> Int
part2 input =
  sum (map (testDigits0 12) banks)
  where
    banks = parseInput input
