module Day1 (part1, part2) where
import qualified Data.Text as T

data DialState = DialState
  { count :: Int
  , position :: Int
  }

dialSize :: Int
dialSize = 100

defaultDialState :: DialState
defaultDialState = DialState { count = 0, position = 50 }

parseDialTurn :: T.Text -> Int
parseDialTurn line =
  case T.unpack line of
    'L':amt -> -(read amt)
    'R':amt -> (read amt)
    _ -> error "Failed to parse line"

parseInput :: T.Text -> [Int]
parseInput input =
  map (parseDialTurn) (T.lines input)

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

part1 :: T.Text -> Int
part1 input =
  let
    turns = parseInput input
    DialState { count } = foldl checkZero defaultDialState turns
  in
    count

part2 :: T.Text -> Int
part2 input =
  let
    turns = parseInput input
    DialState { count } = foldl checkClick defaultDialState turns
  in
    count
