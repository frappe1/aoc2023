import Data.List.Split ( splitOneOf, splitOn )
import Data.Char (digitToInt, isDigit, isAlpha)
import Data.ByteString (find)
import Data.List(inits)
import Text.Printf (errorBadArgument)

maxRed = 12 :: Int
maxGreen = 13 :: Int
maxBlue = 14 :: Int

part1 :: IO()
part1 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

testLine :: String
testLine = "6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

testLine2 :: String
testLine2 = "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

checkLine :: String -> Bool
checkLine s = checkCubeAmount $ concat $ parsedInput cubeString
  where dividedString = splitOn ":" s
        gameString = head dividedString
        cubeString = last dividedString

testOutput2 :: String -> [Int]
testOutput2 s = findFewest $ parsedInput s

parsedInput :: String -> [[(String, Int)]]
parsedInput s = map stringToTuple (splitOn "; " (last $ splitOn ":" s))

stringToTuple :: String -> [(String, Int)]
stringToTuple s = [(filter (isAlpha) x, read $ filter isDigit x) | x <- (splitOn ", " s)]

test :: IO()
test = do
  print $ testOutput2 testLine

part2 :: IO()
part2 = do
  contents <- readFile "input.txt"
  print $ part2solution contents

part1solution :: String -> Int
part1solution input = sum $ map (read . (filter isDigit) . head . splitOn ":") $ filter checkLine (lines input)

checkCubeAmount :: [(String, Int)] -> Bool
checkCubeAmount (("red", x):xs) | x <= maxRed = checkCubeAmount xs
                                | otherwise = False
checkCubeAmount (("green", x):xs) | x <= maxGreen = checkCubeAmount xs
                                | otherwise = False
checkCubeAmount (("blue", x):xs) | x <= maxBlue = checkCubeAmount xs
                                | otherwise = False     
checkCubeAmount [] = True                         
checkCubeAmount _ = errorBadArgument

part2solution :: String -> Int
part2solution input = sum $ map (product . findFewest . parsedInput) (lines input)

findFewest :: [[(String, Int)]] -> [Int]
findFewest s = [findMaxRed $ concat s, findMaxGreen $ concat s, findMaxBlue $ concat s]

--Finds the largest red value in a certain line
findMaxRed :: [(String, Int)] -> Int
findMaxRed xs = maximum [snd t | t <- xs, fst t == "red"]

--Finds the largest green value in a certain line
findMaxGreen :: [(String, Int)] -> Int
findMaxGreen xs = maximum [snd t | t <- xs, fst t == "green"]

--Finds the largest blue value in a certain line
findMaxBlue :: [(String, Int)] -> Int
findMaxBlue xs = maximum [snd t | t <- xs, fst t == "blue"]