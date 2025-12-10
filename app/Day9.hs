{-# LANGUAGE OverloadedStrings #-}
module Day9 (Day9Puzzle(..)) where
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Util

type Point = (Int, Int)
data Day9Puzzle = Day9Puzzle [Point]

type CompressedMap = Map.Map Point Point
type OutsideMap = Set.Set Point

parsePoint :: T.Text -> Maybe Point
parsePoint text = do
  a:b:[] <- pure ((read . T.unpack) <$> (T.splitOn "," text))
  return (a, b)

getAllPairs :: [a] -> [(a, a)]
getAllPairs [] = []
getAllPairs (x:xs) = ((x,) <$> xs) <> (getAllPairs xs)

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (succ $ abs (x2 - x1)) * (succ $ abs (y2 - y1))

compress :: Ord a => [a] -> Map.Map a Int
compress xs =
  foldr (uncurry Map.insert) Map.empty $ zip (L.nub $ L.sort xs) [1..]

compressPoints :: [Point] -> CompressedMap
compressPoints xs =
  case sequence (lookupPoint <$> xs) of
    Just m -> foldr (uncurry Map.insert) Map.empty m
    Nothing -> Map.empty
  where
    xPoints = compress (fst <$> xs)
    yPoints = compress (snd <$> xs)
    lookupPoint p@(x :: Int, y :: Int) = do
      nx <- Map.lookup x xPoints
      ny <- Map.lookup y yPoints
      return (p, (nx, ny))

floodFill :: Point -> OutsideMap -> OutsideMap -> OutsideMap
floodFill p@(x, y) m acc
  | x < 0 || x > 300 || y < 0 || y > 300 || (Set.member p m) || (Set.member p acc) = acc
  | otherwise = foldr (flip floodFill m) (Set.insert p acc) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

range :: (Ord a, Enum a) => a -> a -> [a]
range a b
  | a < b = [a..b]
  | otherwise = [b..a]

drawLine :: Point -> Point -> OutsideMap -> OutsideMap
drawLine (x1, y1) (x2, y2) m
  | x1 == x2 = foldr Set.insert m ((x1,) <$> range y1 y2)
  | y1 == y2 = foldr Set.insert m ((,y1) <$> range x1 x2)
  | otherwise = error "line must be axis-aligned"

drawLines' :: Point -> [Point] -> OutsideMap -> OutsideMap
drawLines' a (b:[]) m = drawLine b a m
drawLines' f (a:b:xs) m = drawLines' f (b:xs) (drawLine a b m)
drawLines' _ _ _ = error "missing points"

drawLines :: [Point] -> OutsideMap
drawLines (x:xs) = drawLines' x (x:xs) Set.empty
drawLines _ = error "missing points"

pointsInBox :: Point -> Point -> [Point]
pointsInBox (x1, y1) (x2, y2) =
  [(x, y) | x <- range x1 x2, y <- range y1 y2]

boxInCompressedMap :: Point -> Point -> CompressedMap -> OutsideMap -> Bool
boxInCompressedMap p q compressedMap m =
  case sequence $ ((flip Map.lookup compressedMap) <$> [p, q]) of
    Just [a, b] ->
      not (any (flip Set.member m) xs)
      where xs = pointsInBox a b
    _ -> False

instance Puzzle Day9Puzzle where
  parseInput input = do
    let inputLines = T.lines input
    points <- sequence $ parsePoint <$> inputLines
    return (Day9Puzzle points)

  part1 (Day9Puzzle points) =
    L.maximum (uncurry area <$> getAllPairs points)

  part2 (Day9Puzzle points) =
    let
      compressedMap = compressPoints points
      compressedPoints = (\a -> Map.findWithDefault (0, 0) a compressedMap) <$> points
      outsidePoints = floodFill (0, 0) (drawLines compressedPoints) Set.empty
    in
      L.maximum (uncurry area <$> (filter (\(p, q) -> boxInCompressedMap p q compressedMap outsidePoints) (getAllPairs points)))
      
