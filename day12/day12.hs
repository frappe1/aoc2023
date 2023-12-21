import Data.List.Split (splitOn)
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
  print contents

testLine :: String
testLine = ".????????..??.#?."

testLine2 :: String
testLine2 = "..#...#"

part1solution :: String -> Int
part1solution input = sum $ map (uncurry placeAllGroups) (parseInput input)

part2solution :: String -> Int
part2solution input = undefined

parseInput :: String -> [(String, [Int])]
parseInput input = map parseRow (lines input)
  where parseRow row = (head $ words row, map read $ splitOn "," (words row !! 1))

tryPlace :: String -> Int -> Int -> Bool
tryPlace line x g = noHashtagsBefore && before && during && after
                     where 
                           noHashtagsBefore = notElem 'd' $ deprecateLine (take x line)
                           before | x == 0 = True
                                  | otherwise = notElem (line !! (x-1)) "-#x"
                           during | x + g > length line = False
                                  | otherwise = all (\x -> x /= '.' && x /= 'x' && x /= '-') $ take g (drop x line)
                           after  | x + g > length line = False
                                  | x + g == length line = True
                                  | otherwise = notElem (line !! (x + g)) "-#x"

place :: String -> Int -> Int -> String
place line x g = deprecateLine (take x line) ++ replicate g 'x' ++ "." ++ drop (x + g + 1) line

deprecateLine :: String -> String
deprecateLine line = map deprecateChar line
  where deprecateChar '#' = 'd'
        deprecateChar 'x' = 'x'
        deprecateChar _ = '-'

placeAllGroups :: String -> [Int] -> Int
placeAllGroups line [g] = sum [f (notElem 'd' (place line x g) && (notElem '#' (drop (x+g) line))) | x <- [(length (takeWhile (=='-') line))..length line - 1], tryPlace line x g]
  where f True = 1
        f False = 0
placeAllGroups line (g:gs) = sum [placeAllGroups (place line x g) gs | x <- [(length (takeWhile (=='-') line))..length line - 1], tryPlace line x g]
placeAllGroups _ _ = error "bad argument"