import Data.List.Split ( splitOneOf, splitOn )
import Data.Char (digitToInt, isDigit, isAlpha)
import Data.ByteString (find)
import Data.List(inits, sort, sortBy)
import Text.Printf (errorBadArgument)
import Data.List.NonEmpty (groupWith)
import Data.Maybe (isNothing, isJust)
import GHC.Exception (errorCallException)

part1 :: IO()
part1 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

part2 :: IO()
part2 = do
  contents <- readFile "input.txt"
  print $ parseInput2 contents
  print $ part2solution contents

test :: IO()
test = do
  contents <- readFile "input.txt"
  print $ convertInput testTable (2,9)

part1solution :: String -> Int
part1solution input = minimum $ map (findLocation (snd $ parseInput input)) (fst $ parseInput input)

parseInput :: String -> ([Int], [[(Int, Int, Int)]])
parseInput input = (parseSeeds (head splittedInput), map parseTable $ tail splittedInput)
  where rows = lines input :: [String]
        splittedInput = splitOn [""] rows

parseSeeds :: [String] -> [Int]
parseSeeds line = map read $ filter (\x -> isDigit $ head x) $ words $ head line

parseTable :: [String] -> [(Int, Int, Int)]
parseTable rows = map parseRow $ tail rows
  where 
    parseRow :: String -> (Int,Int, Int)
    parseRow row = ((numbers row) !! 1, head (numbers row), numbers row !! 2)
    numbers row = map read $ words row :: [Int]

findLocation :: [[(Int, Int, Int)]] -> Int -> Int
findLocation [] id = id
findLocation (table:tables) id | length findEntry > 0 = findLocation tables (head findEntry)
                                | otherwise = findLocation tables id
  where
        findEntry = [id + (d-s) | (s,d,r) <- table, id >= s, id <= s+r]

part2solution :: String -> Int
part2solution input = minimum $ map (findLocation2 tables) (seeds)
  
--part2solution input = convertInput (tables !! 1) (92,1)
  where tables = snd $ parseInput2 input
        seeds = fst $ parseInput2 input

parseSeeds2 :: [String] -> [(Int, Int)]
parseSeeds2 line = toTuple $ map read $ filter (\x -> isDigit $ head x) $ words $ head line
  where toTuple :: [Int] -> [(Int, Int)]
        toTuple (x:y:ys) = (x, y) : toTuple ys
        toTuple [] = []
        toTuple _ = errorBadArgument

-- Tables -> (first seed, range) -> [(dest value of first seed, range), ...]
findLocation2 :: [[(Int, Int, Int)]] -> (Int, Int) -> Int
findLocation2 [] id = fst id
findLocation2 (table:tables) id = minimum (map (findLocation2 tables) (convertInput table id))

testTable :: [(Int, Int, Int)]
testTable = [(3,10,7)]

testTable2 :: [(Int, Int, Int)]
testTable2 = [(1,10,3), (5,6,4)]

lookupTable :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int, Int)]
lookupTable (entry:table) input@(ss, rs) = case checkEntry entry input of
                                              Nothing -> lookupTable table input
                                              Just createdTuple -> createdTuple : lookupTable table input
lookupTable [] _ = []

convertInput :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
convertInput [] (s,r) = [(s,r)]
convertInput entries input@(s,r) = map (\(a,b,c) -> (b,c)) $ fillInStart entries input $ fillInBlanks (sortedFragments entries input) input ++ (sortedFragments entries input)
--convertInput entries input@(s,r) = fillInBlanks (sortedFragments entries input) (input)

fillInStart entries input@(s,d) list = case (sortedFragments entries input) of
                              ((sf,_,_)):_ -> case (signum (sf-s)) of
                                                  1 -> (s, s, sf - s) : list
                                                  otherwise -> list
                              otherwise -> list
sortedFragments entries@((sf,_,_):_) input = sortBy (\(a,_,_) (b,_,_) -> compare a b) $ lookupTable entries input
sortedFragments _ _ = []

fillInBlanks :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int, Int)]
fillInBlanks ((s1, _, r1):(s2, _, r2):bs) (s, r) | s1 + r1 == s2 = fillInBlanks ((s2, 0, r2):bs) (s,r)
                                                   | otherwise = (s1+r1, s1+r1, s2-(s1+r1)) : fillInBlanks ((s2, 0, r2):bs) (s,r)
fillInBlanks ((s1, _, r1):bs) (s, r) | s1+r1 == s+r = []
                                     | otherwise = [(s1+r1, s1+r1, s+r-(s1+r1))]
fillInBlanks [] (s,r) = [(s,s,r)]                      

checkEntry :: (Int, Int, Int) -> (Int, Int) -> Maybe (Int, Int, Int)
checkEntry (st, dt, rt) (ss, rs) | overlap <= 0 = Nothing
                                 | otherwise = Just (startOverlap, startOverlap + diff, overlap)
  where startOverlap = max st ss
        endOverlap = min (st + rt) (ss+rs - 1)
        overlap = endOverlap - startOverlap
        diff = dt - st

parseInput2 :: String -> ([(Int, Int)], [[(Int, Int, Int)]])
parseInput2 input = (parseSeeds2 (head splittedInput), map parseTable $ tail splittedInput)
  where rows = lines input :: [String]
        splittedInput = splitOn [""] rows