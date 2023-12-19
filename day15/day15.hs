import Data.List.Split(splitOn)

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
  print $ processInput $ parseInput contents

part1solution :: String -> Int
part1solution input = undefined

part2solution :: String -> Int
part2solution input = undefined

parseInput :: String -> [String]
parseInput input = splitOn "," input

processInput :: [String] -> Int
processInput (l:ls) = processString 0 l + processInput ls
  where processString :: Int -> [Char] -> Int
        processString cVal (c:cs) = processString (alg (cVal+(fromEnum c))) cs
        processString cVal [] = cVal
        alg c = mod (c*17) 256
processInput [] = 0