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
  print $ extraPolateValue ((parseInput contents) !! 2)

part1solution :: String -> Int
part1solution input = (sum . map extraPolateValue) (parseInput input)

part2solution :: String -> Int
part2solution input = (sum . map (extraPolateValue . reverse)) (parseInput input)

parseInput :: String -> [[Int]]
parseInput input = map (map read . words) (lines input)

extraPolateValue :: [Int] -> Int 
extraPolateValue vals | all (==0) vals = 0
                      | otherwise = last vals + (extraPolateValue (findDiff vals))

findDiff :: [Int] -> [Int]
findDiff (a:b:bs) = (b-a):(findDiff ((b:bs)))
findDiff _ = []