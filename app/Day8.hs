{-# LANGUAGE OverloadedStrings #-}
module Day8 (Day8Puzzle(..)) where
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.STRef
import Control.Monad.ST
import Util

type Point = (Int, Int, Int)
data Day8Puzzle = Day8Puzzle [(Point, Point)] (Network Point)

data Network a = Network
  { nwIds :: Map.Map a a
  , nwDisjointCount :: Int
  }

networkNew :: Network a
networkNew = Network
  { nwIds = Map.empty
  , nwDisjointCount = 0
  }

networkAdd :: Ord a => a -> Network a -> Network a
networkAdd point nw =
 Network (Map.insert point point (nwIds nw)) (succ (nwDisjointCount nw))

networkParent :: Ord a => Network a -> a -> a
networkParent Network { nwIds } i =
  case Map.lookup i nwIds of
    Nothing -> i
    Just p -> p

networkRoot :: Ord a => Network a -> a -> (Network a, a)
networkRoot nw i
  | p == i = (nw, i)
  | otherwise = networkRoot (nw { nwIds = (Map.insert i gp (nwIds nw)) }) p
  where
    p = networkParent nw i
    gp = networkParent nw p

networkUnion :: Ord a => Network a -> a -> a -> Network a
networkUnion nw p q
  | i == j = nw''
  | otherwise =
    Network (Map.insert i j . nwIds $ nw'') (pred . nwDisjointCount $ nw'')
  where
    (nw', i) = networkRoot nw p
    (nw'', j) = networkRoot nw' q

networkRoots :: Ord a => Network a -> [a]
networkRoots nw = runST $ do
  state <- newSTRef nw
  mapM (getNode state) $ (Map.keys . nwIds) nw
  where
    getNode networkState node = do
      network <- readSTRef networkState
      let (network', root) = networkRoot network node
      writeSTRef networkState network'
      return root

parsePoint :: T.Text -> Maybe Point
parsePoint text = do
  a:b:c:[] <- pure ((read . T.unpack) <$> (T.splitOn "," text))
  return (a, b, c)

pointDistance :: Point -> Point -> Int
pointDistance (x1, y1, z1) (x2, y2, z2) =
  ((dx * dx) + (dy * dy) + (dz * dz))
  where
    dx = x2 - x1
    dy = y2 - y1
    dz = z2 - z1

getAllPairs :: [a] -> [(a, a)]
getAllPairs [] = []
getAllPairs (x:xs) =
  ((\a -> (x, a)) <$> xs) <> (getAllPairs xs)

getAllDistances :: [Point] -> [(Point, Point)]
getAllDistances points =
  snd <$> (L.sort $ (\p -> (uncurry pointDistance p, p)) <$> (getAllPairs points))

makeAllButLastConnection :: [(Point, Point)] -> Network Point -> (Point, Point)
makeAllButLastConnection [] _ = ((0, 0, 0), (0, 0, 0))
makeAllButLastConnection ((a, b):xs) nw
  | i == j = makeAllButLastConnection xs nw''
  | (nwDisjointCount nw'') == 2 = (a, b)
  | otherwise = makeAllButLastConnection xs (networkUnion nw'' i j)
  where
    (nw', i) = networkRoot nw a
    (nw'', j) = networkRoot nw' b

instance Puzzle Day8Puzzle where
  parseInput input = do
    let inputLines = T.lines input
    points <- sequence $ parsePoint <$> inputLines
    return (Day8Puzzle (getAllDistances points) (foldr networkAdd networkNew points))

  part1 (Day8Puzzle distances network) =
    product $ take 3 $ reverse $ L.sort $ length <$> (L.group $ L.sort roots)
    where
      firstConnections = take 1000 $ distances
      roots = networkRoots $ foldl (uncurry . networkUnion) network firstConnections

  part2 (Day8Puzzle distances network) =
    let
      (a, b) = makeAllButLastConnection distances network
      (x1, _, _) = a
      (x2, _, _) = b
    in
      x1 * x2
