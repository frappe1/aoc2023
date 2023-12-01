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

part1solution :: String -> Int
part1solution input = undefined

part2solution :: String -> Int
part2solution input = undefined