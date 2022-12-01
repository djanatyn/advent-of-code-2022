module Day01 where

import Data.Coerce (coerce)
import Data.List (sort)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | Food has calories.
newtype Calories = Calories Int deriving (Show)

-- | Elves hold food.
data ElfInventory where
  ElfInventory :: {food :: [Calories]} -> ElfInventory
  deriving (Show)

-- | Read a text file as a list of strings.
inputLines :: FilePath -> IO [String]
inputLines path = lines <$> readFile path

-- | Given a set of lines, parse as a list of elf inventories, with each elf
-- | separated by empty lines.
parse :: [ElfInventory] -> [String] -> [ElfInventory]
parse accum lines =
  let (items, rest) = span (/= "") lines -- split on empty lines
      elf = ElfInventory {food = Calories . read @Int <$> items}
   in case rest of
        [] -> elf : accum
        _ -> parse (elf : accum) (tail rest)

-- | Problem input.
problemPath :: FilePath
problemPath = "input/day01.txt"

-- | Example from problem statement.
examplePath :: FilePath
examplePath = "input/example01.txt"

-- | Solve the example, expecting the stated solution.
testExample :: TestTree
testExample =
  testGroup
    "examples"
    [ testCase "example part one" $ do
        solution <- solvePartOne examplePath
        solution @?= 24000,
      testCase "example part two" $ do
        solution <- solvePartTwo examplePath
        solution @?= 45000
    ]

-- | Find the Elf carrying the most Calories. How many total Calories is that
-- | Elf carrying?
solvePartOne :: FilePath -> IO Int
solvePartOne path = do
  lines <- inputLines path
  let elves = parse [] lines
      total = totalCalories <$> elves
   in pure $ maximum total

-- | Find the Elf carrying the most Calories. How many total Calories is that
-- | Elf carrying?
solvePartTwo :: FilePath -> IO Int
solvePartTwo path = do
  lines <- inputLines path
  let elves = parse [] lines
      total = totalCalories <$> elves
   in pure . sum . take 3 . reverse $ sort total

-- | Sum of the calories of all the food an elf is carrying.
totalCalories :: ElfInventory -> Int
totalCalories ElfInventory {food} = sum $ coerce <$> food

-- | Run tests.
test :: IO ()
test =
  defaultMain $
    testGroup "examples" [testExample]

-- | Solve problem.
main :: IO ()
main = do
  partOne <- solvePartOne problemPath
  print $ unwords ["part one:", show partOne]
  partTwo <- solvePartTwo problemPath
  print $ unwords ["part two:", show partTwo]
