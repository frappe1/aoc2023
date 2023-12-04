import Data.List.Split ( splitOneOf, splitOn )
import Data.Char (digitToInt, isDigit, isAlpha)
import Data.ByteString (find)
import Data.List(inits, sort)
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
  --print $ winCards (findWinsPerCard $ parseInput contents) [1,2,4,4,2,1] 2
  print $ take 2 $ drop 3 numberOfCards
    where numberOfCards = [1,2,4,4,2,1]

testLine :: String
testLine = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

parseWinning :: String ->  [Int]
parseWinning line = map read $ filter (keepNumbers) $ splitOn " " $ takeWhile (/= '|') $ dropWhile (/= ':') line
  where keepNumbers "" = False
        keepNumbers xs = isDigit $ head xs 

parseNumbers :: String -> [Int]
parseNumbers line = map read $ filter keepNumbers $ splitOn " " $ dropWhile (/= '|') line
  where keepNumbers "" = False
        keepNumbers xs = isDigit $ head xs

part1solution :: String -> Int
part1solution input = sum $ map (points . getOverlap) cards
  where 
    getOverlap :: ([Int], [Int]) -> Int
    getOverlap (ws, ns) = length $ filter (\n -> elem n ws) ns
    points 0 = 0
    points x = 2^(x-1)
    cards = parseInput input

parseInput :: String -> [([Int], [Int])]
parseInput input = map parseCard $ lines input
  where parseCard line = (parseWinning line, parseNumbers line)

findWinsPerCard :: [([Int],[Int])] -> [Int]
findWinsPerCard cards = map getOverlap cards
    where getOverlap (ws, ns) = length $ filter (\n -> elem n ws) ns

part2solution :: String -> Int
part2solution input = scratch (findWinsPerCard $ parseInput input) (replicate (length $ parseInput input) 1) 0

--scratch2 :: [([Int], [Int])] -> Int -> [Int]
--scratch2 allCards index = index : concat [scratch allCards newCardIndex | newCardIndex <- newCardIndexes]
 -- where newCardIndexes | overlap > 0 = [index + n | n <- [1..overlap]]
   --               | otherwise = []
     --   getOverlap :: ([Int], [Int]) -> Int
       -- getOverlap (ws, ns) = length $ filter (\n -> elem n ws) ns
       -- overlap = getOverlap (allCards !! index)

scratch :: [Int] -> [Int] -> Int -> Int
scratch winsPerCard numberOfCards index | index >= length numberOfCards = sum numberOfCards
                                        | winsPerCard !! index == 0 = scratch winsPerCard numberOfCards (index + 1)
                                        | otherwise = scratch winsPerCard (winCards winsPerCard numberOfCards index) (index + 1)

winCards :: [Int] -> [Int] -> Int -> [Int]
winCards winsPerCard numberOfCards index = list1 ++ map (\x -> x + (numberOfCards !! index)) list2 ++ list3
    where list1 = take (index + 1) numberOfCards
          list2 = take (winsPerCard !! index) (drop (index+1) numberOfCards)
          list3 = drop (index + 1 + (winsPerCard !! index)) numberOfCards