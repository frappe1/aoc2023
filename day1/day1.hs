import Data.List.Split ( splitOneOf )
import Data.Char (digitToInt, isDigit)
import Data.ByteString (find)
import Data.List(inits)
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
  print $ map part2helper $ parseInput contents

part1solution :: String -> Int
part1solution input = sum $ map oneLine1 $ parseInput input

parseInput :: String -> [String]
parseInput = splitOneOf " \n\r"

oneLine1 :: String -> Int
oneLine1 line = (head digits * 10) + last digits
  where
    digits = [digitToInt c | c <- line, isDigit c] :: [Int]

part2solution :: String -> Int
part2solution input = sum $ map part2helper $ parseInput input

part2helper :: String -> Int
part2helper line = (head xs * 10) + last xs
  where xs = extractDigits line

extractDigits :: String -> [Int]
extractDigits (c:cs) | isDigit c = digitToInt c : extractDigits cs
                       | d > 0 = d : extractDigits cs
                       | otherwise = extractDigits cs
                        where d = findDigit (c:cs)
extractDigits [] = []

findDigit :: String -> Int
findDigit line | elem "one" s = 1
                | elem "two" s = 2
                | elem "three" s = 3
                | elem "four" s = 4
                | elem "five" s = 5
                | elem "six" s = 6
                | elem "seven" s = 7
                | elem "eight" s = 8
                | elem "nine" s = 9
                | otherwise = -1
    where s = inits line