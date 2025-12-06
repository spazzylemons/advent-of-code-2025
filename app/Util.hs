module Util (Puzzle(..)) where
import qualified Data.Text as T

class Puzzle a where
  parseInput :: T.Text -> Maybe a
  part1 :: a -> Int
  part2 :: a -> Int
