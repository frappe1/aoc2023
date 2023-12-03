import Data.List.Split ( splitOneOf, splitOn )
import Data.Char (digitToInt, isDigit, isAlpha)
import Data.ByteString (find)
import Data.List(inits)
import Text.Printf (errorBadArgument)

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
  print $ fst $ parseInput contents

testLine :: String
testLine = "...*......"

part1solution :: String -> Int
part1solution input = sum $ map takeValue $ filter (nearSymbol symbols) numbers
  where (numbers, symbols) = parseInput input
        takeValue (_, _, val) = val

parseInput :: String -> ([(Int, Int, Int)], [(Int, Int)])
parseInput input = (concat [parseNumbers row 0 ypos [] | (row, ypos) <- zip rows [0,1..]], concat [parseSymbols row 0 ypos | (row, ypos) <- zip rows [0,1..]])
  where rows = lines input

parseSymbols :: String -> Int -> Int -> [(Int, Int)]
parseSymbols (x:xs) xpos ypos | isDigit x || x == '.' = parseSymbols xs (xpos+1) ypos
                              | otherwise = (xpos, ypos) : parseSymbols xs (xpos+1) ypos
parseSymbols _ _ _ = []

parseNumbers :: String -> Int -> Int -> String -> [(Int, Int, Int)]
parseNumbers (x:xs) xpos ypos [] | isDigit x = parseNumbers xs (xpos+1) ypos [x]
                                  | otherwise = parseNumbers xs (xpos+1) ypos []
parseNumbers (x:xs) xpos ypos acc | isDigit x = parseNumbers xs (xpos+1) ypos (x:acc)
                                  | otherwise = (xpos - length acc, ypos, (read . reverse) acc):parseNumbers xs (xpos+1) ypos []
parseNumbers [] xpos ypos acc | length acc > 0 = [(xpos - length acc, ypos, (read . reverse) acc)]
                              | otherwise = []

withinRange :: (Int, Int) -> (Int, Int) -> Bool
withinRange (x1,y1) (x2,y2) = abs (x1-x2) <= 1 && abs (y1-y2) <= 1

nearSymbol :: [(Int, Int)] -> (Int, Int, Int) -> Bool
nearSymbol ((xposS, yposS):symbols) number@(xposN, yposN, value) | or [withinRange (xposN + n, yposN) (xposS, yposS) | n <- [0..(len-1)]] = True
                                                            | otherwise = nearSymbol symbols number
  where len = length $ show value
nearSymbol _ _ = False

nearNumbers :: [(Int, Int, Int)] -> (Int, Int) -> [Int]
nearNumbers ((xposN, yposN, val):ns) s@(xposS, yposS) | or [withinRange (xposN + n, yposN) (xposS, yposS) | n <- [0..(len-1)]] = val : nearNumbers ns s
                                                      | otherwise = nearNumbers ns s
                                            where len = length $ show val
nearNumbers _ _ = []

part2solution :: String -> Int
part2solution input = sum $ map gearRatio $ filter (\s -> (length $ nearNumbers numbers s) == 2) symbols
  where (numbers, symbols) = parseInput input
        gearRatio s = product $ nearNumbers numbers s