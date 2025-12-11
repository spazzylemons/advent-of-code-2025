{-# LANGUAGE OverloadedStrings #-}
module Day10 (Day10Puzzle(..)) where
import qualified Data.List as L
import qualified Data.IntSet as IntSet
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Bits (setBit, xor, testBit)
import System.IO
import System.IO.Unsafe
import System.Process
import Util

data Machine = Machine Int [Int] [Int] deriving (Show, Eq)
newtype Day10Puzzle = Day10Puzzle [Machine]

removeBrackets :: T.Text -> Maybe T.Text
removeBrackets text = do
  (_, text1) <- T.uncons text
  (text2, _) <- T.unsnoc text1
  return text2

intsToBits :: (Foldable t) => t Int -> Int
intsToBits xs =
  foldr (flip setBit) 0 xs

parseLights :: T.Text -> Maybe Int
parseLights text = do
  text1 <- removeBrackets text
  return (intsToBits (L.elemIndices '#' (T.unpack text1)))

parseJoltages :: T.Text -> Maybe [Int]
parseJoltages text = do
  text1 <- removeBrackets text
  return $ (read . T.unpack) <$> (T.splitOn "," text1)

parseToggle :: T.Text -> Maybe Int
parseToggle text = do
  values <- parseJoltages text
  return (intsToBits values)

parseMachine :: T.Text -> Maybe Machine
parseMachine line = do
  (head', tail') <- L.uncons $ T.splitOn " " line
  (init', last') <- L.unsnoc tail'
  lights <- parseLights head'
  toggles <- sequence $ parseToggle <$> init'
  joltages <- parseJoltages last'
  return $ Machine lights toggles joltages

searchButtonPressesPt1 :: IntSet.IntSet -> [Int] -> Int -> Int
searchButtonPressesPt1 lights toggles acc
  | any (==0) (IntSet.toList lights) = acc
  | otherwise = searchButtonPressesPt1 newLights toggles (succ acc)
  where
    newLights = IntSet.fromList [l `xor` t | l <- (IntSet.toList lights), t <- toggles]

fewestButtonPressesPt1 :: Machine -> Int
fewestButtonPressesPt1 (Machine lights toggles _) =
  searchButtonPressesPt1 (IntSet.singleton lights) toggles 0

variableName :: Int -> String
variableName i =
  'x':(show i)

createLpText :: Machine -> String
createLpText (Machine _ toggles joltages) =
  let
    variables = variableName <$> [1..(length toggles)]
    createJoltage joltage index =
      (L.intercalate " + " (fst <$> (filter (\(_, b) -> testBit b index) (zip variables toggles)))) ++ " = " ++ (show joltage) ++ "\n"
  in (
    "minimize\n" ++
    (L.intercalate " + " variables) ++ "\n" ++
    "subject to\n" ++
    (mconcat [createJoltage joltage i | (joltage, i) <- zip joltages [0..]]) ++
    "bounds\n" ++
    (mconcat [var ++ ">=0\n" | var <- variables]) ++
    "integer\n" ++
    (mconcat [var ++ "\n" | var <- variables]) ++
    "end\n"
  )

solveMachineWithIO :: Machine -> IO String
solveMachineWithIO machine = do
  (Just hin, Just hout, _, phdl) <- createProcess (shell "glpsol --cpxlp /dev/stdin -o /dev/stdout")
    { std_in  = CreatePipe
    , std_out = CreatePipe
    }
  hPutStr hin (createLpText machine)
  hClose hin
  content <- hGetContents hout
  _ <- waitForProcess phdl
  return content

solveMachine :: Machine -> Int
solveMachine machine =
  firstResult
  where
    resultText = unsafePerformIO (solveMachineWithIO machine)
    resultLines = catMaybes ((\a -> (T.stripPrefix "Objective:  obj = " a) >>= (T.stripSuffix " (MINimum)")) <$> (T.lines (T.pack resultText)))
    firstResult = case resultLines of
      [] -> error "failed"
      (x:_) -> read (T.unpack x)

instance Puzzle Day10Puzzle where
  parseInput input = do
    machines <- sequence $ parseMachine <$> T.lines input
    return $ Day10Puzzle machines

  part1 (Day10Puzzle machines) =
    sum (fewestButtonPressesPt1 <$> machines)

  part2 (Day10Puzzle machines) =
    sum (solveMachine <$> machines)
