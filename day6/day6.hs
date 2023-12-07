import Data.List.Split ( splitOneOf, splitOn )
import Data.Char (digitToInt, isDigit, isAlpha)
import Data.ByteString (find)
import Data.List(inits, sort, sortBy, transpose)
import Text.Printf (errorBadArgument)
import Data.List.NonEmpty (groupWith)
import Data.Maybe (isNothing, isJust)
import GHC.Exception (errorCallException)
import GHC.Float

part1 :: IO()
part1 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

part2 :: IO()
part2 = do
  contents <- readFile "input.txt"
  print $ part2solution contents

test :: IO()
test = do
  contents <- readFile "input.txt"
  print $ parseInput contents

part1solution :: String -> Int
part1solution input = product $ map (uncurry getWidth) (parseInput input)

parseInput :: String -> [(Int, Int)]
parseInput input = map (\[a,b] -> (read a, read b)) $ transpose $ map (tail . words) $ lines input

getWidth :: Int -> Int -> Int
getWidth t record = t + 1- 2 * (threshold + 1)
  where tf :: Double
        tf = int2Double t
        threshold = floor (tf/2 - sqrt (tf*tf/4-int2Double record))

part2solution :: String -> Int
part2solution input = uncurry getWidth (parseInput2 input)

parseInput2 :: String -> (Int, Int)
parseInput2 input = (head toList, toList !! 1)
  where toList = map read $ map concat $ map (tail . words) $ lines input