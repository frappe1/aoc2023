import Data.List.Split (splitOn)
import Data.Maybe
import Data.List (transpose)

data Rock = Empty | Round | Square deriving Ord

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
  --putStr $ showMatrix $ (drop (abs $ length (mirror contents) - 2*4) $ take 4 (mirror contents))
    --where mirror contents = (parseInput contents) !! 1
  print $ findReflection 1 $ (parseInput contents) !! 0

part1solution :: String -> Int
part1solution input = sum $ map (score . findReflection 0) $ parseInput input
  where score (x, False) = 100 * x
        score (x, True) = x

part2solution :: String -> Int
part2solution input = sum $ map (score . findReflection 1) $ parseInput input
  where score (x, False) = 100 * x
        score (x, True) = x

parseInput :: String -> [[[Char]]]