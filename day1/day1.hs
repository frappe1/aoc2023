part1 :: IO()
part1 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

part2 :: IO()
part2 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

part1solution :: String -> Int
part1solution = undefined

part2solution :: String -> Int
part2solution = undefined