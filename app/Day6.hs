{-# LANGUAGE OverloadedStrings #-}
module Day6 (Day6Puzzle(..)) where
import qualified Data.List as L
import Data.List.Split (splitWhen)
import qualified Data.Text as T
import Util

type Operator = [Int] -> Int

data Equation = Equation [T.Text] Operator

newtype Day6Puzzle = Day6Puzzle [Equation]

parseOperator :: T.Text -> Maybe Operator
parseOperator text =
  case text of
    "+" -> Just sum
    "*" -> Just product
    _ -> Nothing

solveEquation :: Equation -> Int
solveEquation (Equation numbers operator) =
  operator $ read . T.unpack <$> numbers

solveEquations :: [Equation] -> Int
solveEquations equations =
  sum $ solveEquation <$> equations

splitLines :: [String] -> [[String]]
splitLines = splitWhen $ all (==' ')

splitInput :: [T.Text] -> [[T.Text]]
splitInput textLines =
  (T.pack <$>) <$> (L.transpose <$> (splitLines $ L.transpose $ T.unpack <$> textLines))

instance Puzzle Day6Puzzle where
  parseInput input = do
    (lastLine:otherLines) <- pure $ reverse (T.lines input)
    operators <- sequence $ parseOperator <$> (T.words lastLine)
    let numberLists = splitInput $ reverse otherLines
    let equations = zipWith Equation numberLists operators
    return (Day6Puzzle equations)

  part1 (Day6Puzzle equations) =
    solveEquations equations

  part2 (Day6Puzzle equations) =
    solveEquations (transposeEquation <$> equations)
    where
      transposeEquation (Equation numbers operator) =
        Equation (T.transpose numbers) operator
