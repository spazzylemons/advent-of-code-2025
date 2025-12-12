{-# LANGUAGE OverloadedStrings #-}
module Day11 (Day11Puzzle(..)) where
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Util

type Node = T.Text
type Graph = Map.Map Node [Node]
newtype Day11Puzzle = Day11Puzzle Graph

class Show a => EdgeCounter a where
  ecZero :: a
  ecNew :: Node -> a
  ecAdd :: Node -> a -> a -> a

instance EdgeCounter Int where
  ecZero = 0
  ecNew _ = 1
  ecAdd _ a b = a + b

data DacFftCounter = DacFftCounter
  { dacNfftN :: Int
  , dacYfftN :: Int
  , dacNfftY :: Int
  , dacYfftY :: Int
  } deriving (Show)

instance EdgeCounter DacFftCounter where
  ecZero = DacFftCounter
    { dacNfftN = 0
    , dacYfftN = 0
    , dacNfftY = 0
    , dacYfftY = 0
    }

  ecNew "dac" = DacFftCounter
    { dacNfftN = 0
    , dacYfftN = 1
    , dacNfftY = 0
    , dacYfftY = 0
    }

  ecNew "fft" = DacFftCounter
    { dacNfftN = 0
    , dacYfftN = 0
    , dacNfftY = 1
    , dacYfftY = 0
    }

  ecNew _ = DacFftCounter
    { dacNfftN = 1
    , dacYfftN = 0
    , dacNfftY = 0
    , dacYfftY = 0
    }

  ecAdd "dac" a b = DacFftCounter
    { dacNfftN = 0
    , dacYfftN = (dacYfftN a) + (dacNfftN a) + (dacYfftN b) + (dacNfftN b)
    , dacNfftY = 0
    , dacYfftY = (dacYfftY a) + (dacNfftY a) + (dacYfftY b) + (dacNfftY b)
    }

  ecAdd "fft" a b = DacFftCounter
    { dacNfftN = 0
    , dacYfftN = 0
    , dacNfftY = (dacNfftY a) + (dacNfftN a) + (dacNfftY b) + (dacNfftN b)
    , dacYfftY = (dacYfftY a) + (dacYfftN a) + (dacYfftY b) + (dacYfftN b)
    }

  ecAdd _ a b = DacFftCounter
    { dacNfftN = (dacNfftN a) + (dacNfftN b)
    , dacNfftY = (dacNfftY a) + (dacNfftY b)
    , dacYfftN = (dacYfftN a) + (dacYfftN b)
    , dacYfftY = (dacYfftY a) + (dacYfftY b)
    }

type Cache a = Map.Map Node a

parseLine :: Graph -> T.Text -> Maybe Graph
parseLine graph line = do
  (input:outputString:[]) <- pure (T.splitOn ": " line)
  let outputs = T.words outputString
  return $ Map.insert input outputs graph

countRecFold :: EdgeCounter a => Graph -> Node -> STRef s (Cache a) -> a -> Node -> ST s a
countRecFold graph n cache count node = do
  value <- countRec graph cache node
  return (ecAdd n value count)

countRec :: EdgeCounter a => Graph -> STRef s (Cache a) -> Node -> ST s a
countRec graph cache node = do
  c <- (readSTRef cache) >>= (pure . (Map.lookup node))
  case c of
    Just count -> return count
    Nothing -> do
      count <- case Map.lookup node graph of
        Just outputs ->
          foldM (countRecFold graph node cache) ecZero outputs
        Nothing -> pure (ecNew node)
      modifySTRef cache (Map.insert node count)
      return count

countPaths :: EdgeCounter a => Node -> Graph -> a
countPaths start graph = runST $ do
  cache <- newSTRef Map.empty
  countRec graph cache start

instance Puzzle Day11Puzzle where
  parseInput input = do
    puzzle <- foldM parseLine Map.empty (T.lines input)
    return $ Day11Puzzle puzzle

  part1 (Day11Puzzle graph) =
    ((countPaths "you" graph) :: Int)

  part2 (Day11Puzzle graph) =
    (dacYfftY ((countPaths "svr" graph) :: DacFftCounter))
