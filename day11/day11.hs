import Data.List (transpose)
data Space = Galaxy | Empty deriving Eq

instance Show Space where
  show Galaxy = "X"
  show Empty = "."

convertToSpace :: Char -> Space
convertToSpace '.' = Empty
convertToSpace '#' = Galaxy
convertToSpace _ = error "unknown character"

showMatrix :: [[Space]] -> String
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
  print $ findEmptyRows (parseInput contents) 0

part1solution :: String -> Int
part1solution input = sum $ findDistances $ findGalaxies $ transpose $ addRows $ transpose $ addRows $ parseInput input

part2solution :: String -> Int
part2solution input = sum $ findDistances2 (findGalaxies space) 1000000 emptyCols emptyRows
  where space = parseInput input
        emptyRows = findEmptyRows space 0
        emptyCols = findEmptyRows (transpose space) 0

parseInput :: String -> [[Space]]
parseInput input = map (map convertToSpace) $ lines input

addRows :: [[Space]] -> [[Space]]
addRows (x:xs) | all (==Empty) x = x : replicate (length x) Empty : addRows xs
               | otherwise = x:addRows xs
addRows [] = []

findGalaxies :: [[Space]] -> [(Int, Int)]
findGalaxies space = concat [[(x, y) | x <- [0..length (space !! y)-1], (space !! y) !! x == Galaxy] | y <- [0..length space-1]]

findDistances :: [(Int, Int)] -> [Int]
findDistances (g:gs) = map (dist g) gs ++ findDistances gs
  where dist (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)
findDistances [] = []

--PART 2--

findEmptyRows :: [[Space]] -> Int -> [Int]
findEmptyRows (x:xs) acc | all (==Empty) x = acc : findEmptyRows xs (acc+1)
                         | otherwise = findEmptyRows xs (acc+1)
findEmptyRows [] _ = []

findDistances2 :: [(Int, Int)] -> Int -> [Int] -> [Int] -> [Int]
findDistances2 (g:gs) multiplier emptyCols emptyRows = map (dist g) gs ++ findDistances2 gs multiplier emptyCols emptyRows
  where dist (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2) + (multiplier-1) * (length (filter (inBetween x1 x2) emptyCols) + length (filter (inBetween y1 y2) emptyRows))
        inBetween a b c = (c > a && c < b) || (c < a && c > b)
findDistances2 [] _ _ _= []