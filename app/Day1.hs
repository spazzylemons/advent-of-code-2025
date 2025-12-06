module Day1 (Day1Puzzle(..)) where
import qualified Data.Text as T
import Util

data DialState = DialState
  { count :: Int
  , position :: Int
  }

newtype Day1Puzzle = Day1Puzzle [Int]

dialSize :: Int
dialSize = 100

defaultDialState :: DialState
defaultDialState = DialState { count = 0, position = 50 }

parseDialTurn :: T.Text -> Maybe Int
parseDialTurn line =
  case T.unpack line of
    'L':amt -> Just (-(read amt))
    'R':amt -> Just (read amt)
    _ -> Nothing

parseInput1 :: T.Text -> Maybe [Int]
parseInput1 input =
  sequence $ (parseDialTurn <$> (T.lines input))

flipPosition :: Int -> Int -> Int
flipPosition pos turn
  | turn < 0 = (dialSize - pos) `mod` dialSize
  | otherwise = pos

checkZero :: DialState -> Int -> DialState
checkZero (DialState { count, position }) turn =
  let
    newPosition = (position + turn) `mod` dialSize
    newCount = case newPosition of
      0 -> count + 1
      _ -> count
  in DialState
    { count = newCount
    , position = newPosition
    }

checkClick :: DialState -> Int -> DialState
checkClick (DialState { count, position }) turn =
  let
    newPositionNotWrapped = ((flipPosition position turn) + (abs turn))
    (wrapCount, newPosition) = newPositionNotWrapped `divMod` dialSize
    newCount = count + wrapCount
  in DialState
    { count = newCount
    , position = (flipPosition newPosition turn)
    }

instance Puzzle Day1Puzzle where
  parseInput input = do
    turns <- parseInput1 input
    return (Day1Puzzle turns)

  part1 (Day1Puzzle turns) =
    count
    where
      DialState { count } = foldl checkZero defaultDialState turns

  part2 (Day1Puzzle turns) =
    count
    where
      DialState { count } = foldl checkClick defaultDialState turns
