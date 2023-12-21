import Data.List.Split (splitOn)
import Data.Maybe
import Data.List (transpose)

showMatrix :: [[Char]] -> String
showMatrix (c:cs) = show c ++ "\n" ++ showMatrix cs
showMatrix _ = ""

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
parseInput input = splitOn [""] $ lines input

-- (lines to the left/lines above, isVertical)
findReflection :: Int -> [[Char]] -> (Int, Bool)
findReflection wantedDiff mirror | isJust $ testHorizontal mirror = (fromJust $ testHorizontal mirror, False)
                       | isJust $ testVertical mirror = (fromJust $ testVertical mirror, True)
                       | otherwise = error "this shouldn't happen"
                        where testHorizontal :: [[Char]] -> Maybe Int
                              testHorizontal m | length [x | x <- [1..(length m-1)], testReflection x wantedDiff m] > 0 = Just $ head [x | x <- [1..(length m)-1], testReflection x wantedDiff m]
                                               | otherwise = Nothing
                              testVertical m = testHorizontal (transpose m)

testReflection :: Int -> Int -> [[Char]] -> Bool
testReflection x wantedDiff mirror | x <= div (length mirror) 2 = (sum $ zipWith diff (take x mirror) (reverse (take x (drop x mirror)))) == wantedDiff
                        | otherwise = (sum $ zipWith diff (drop x mirror) (reverse (drop (abs $ length mirror - 2*x) $ take x mirror))) == wantedDiff


diff :: [Char] -> [Char] -> Int
diff (a:as) (b:bs) | a == b = diff as bs
                    | otherwise = 1 + diff as bs
diff _ _ = 0