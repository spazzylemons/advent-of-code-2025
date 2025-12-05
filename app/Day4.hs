module Day4 (part1, part2) where
import qualified Data.Text as T
import qualified Data.Set as Set

type Point = (Int, Int)
type Grid = Set.Set Point

parseInput :: T.Text -> Grid
parseInput input =
  parseInput' Set.empty 0 (T.lines input)

parseInput' :: Grid -> Int -> [T.Text] -> Grid
parseInput' grid _ [] = grid
parseInput' grid y (l:xs) =
  let newGrid = parseInput'' grid 0 y (T.unpack l) in
    parseInput' newGrid (succ y) xs

parseInput'' :: Grid -> Int -> Int -> String -> Grid
parseInput'' grid _ _ [] = grid
parseInput'' grid x y ('@':xs) =
  parseInput'' (Set.insert (x, y) grid) (succ x) y xs
parseInput'' grid x y (_:xs) =
  parseInput'' grid (succ x) y xs

neighborPoints :: Point -> [Point]
neighborPoints (x, y) =
  [ (pred x, pred y)
  , (pred x, y)
  , (pred x, succ y)
  , (x, pred y)
  , (x, succ y)
  , (succ x, pred y)
  , (succ x, y)
  , (succ x, succ y)
  ]

countNeighbors :: Grid -> Point -> Int
countNeighbors grid point =
  let neighbors = neighborPoints point in
    length (filter (flip Set.member grid) neighbors)

canBeReachedByForklift :: Grid -> Point -> Bool
canBeReachedByForklift grid point =
  countNeighbors grid point < 4

getRemovable :: Grid -> [Point]
getRemovable grid =
  filter (canBeReachedByForklift grid) (Set.elems grid)

countRemovals :: Grid -> Int -> Int
countRemovals grid acc =
  case getRemovable grid of
    [] -> acc
    removable ->
      let newGrid = foldl (flip Set.delete) grid removable in
        countRemovals newGrid $ length removable + acc

part1 :: T.Text -> Int
part1 input =
  length . getRemovable $ parseInput input

part2 :: T.Text -> Int
part2 input =
  countRemovals (parseInput input) 0
