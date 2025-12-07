module Day7 (Day7Puzzle(..)) where
import qualified Data.List as L
import qualified Data.IntSet as IntSet
import qualified Data.IntMap as IntMap
import qualified Data.Text as T
import Util

data Day7Puzzle = Day7Puzzle Int [IntSet.IntSet]

type TimelineMap = IntMap.IntMap Int

neighbors :: Int -> [Int]
neighbors index = [pred index, succ index]

addNeighbors :: Int -> IntSet.IntSet -> IntSet.IntSet
addNeighbors index beams = foldr IntSet.insert beams (neighbors index)

part1Iter :: (IntSet.IntSet, Int) -> IntSet.IntSet -> (IntSet.IntSet, Int)
part1Iter (beams, splitCount) splitters =
  let
    newSplitters = IntSet.intersection beams splitters
    newSplitCount = splitCount + IntSet.size newSplitters
    newBeams = IntSet.foldr addNeighbors (beams IntSet.\\ newSplitters) newSplitters
  in
    (newBeams, newSplitCount)

addTimelineCount :: Int -> Int -> TimelineMap -> TimelineMap
addTimelineCount count =
  IntMap.alter addCount
  where
    addCount Nothing = Just count
    addCount (Just oldCount) = Just (oldCount + count)

splitTimelines :: IntSet.IntSet -> TimelineMap -> TimelineMap
splitTimelines splitters =
  IntMap.foldlWithKey foldTimelineCounts IntMap.empty
  where
    foldTimelineCounts timelines index count
      | IntSet.member index splitters = foldr (addTimelineCount count) timelines (neighbors index)
      | otherwise = addTimelineCount count index timelines

instance Puzzle Day7Puzzle where
  parseInput input = do
    let inputLines = T.lines input
    (firstLine, _) <- L.uncons inputLines
    start <- T.findIndex (=='S') firstLine
    let splitters = (IntSet.fromList . (L.elemIndices '^') . T.unpack) <$> inputLines
    return (Day7Puzzle start splitters)

  part1 (Day7Puzzle start splitters) =
    snd (foldl part1Iter (IntSet.singleton start, 0) splitters)

  part2 (Day7Puzzle start splitters) =
    sum $ IntMap.elems $ foldl (flip splitTimelines) (IntMap.singleton start 1) splitters
