import Data.List (sortBy, group, sort)
import Data.List.Split (chunksOf)
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
  print $ init [[10],[5,5,5]] ++ [last [[10],[5,5,5]] ++ head [[11]]]

part1solution :: String -> Int
part1solution input = sum $ zipWith (\r (_, b) -> r * b) [1,2..] $ sortBy (\(a,b) (c,d) -> compareHands a c) (parseInput input)

parseInput :: String -> [([Int], Int)]
parseInput input = map (convertToHandAndBid . words) $ lines input
  where 
    convertToHandAndBid :: [String] -> ([Int], Int)
    convertToHandAndBid [h,b] = (map getVal $ chunksOf 1 h, read b)
    convertToHandAndBid _ = error "empty hand"
    getVal :: String -> Int
    getVal s = case s of
                "T" -> 10
                "J" -> 11
                "Q" -> 12
                "K" -> 13
                "A" -> 14
                _ -> read s

part2solution :: String -> Int
part2solution input = sum $ zipWith (\r (_, b) -> r * b) [1,2..] $ sortBy (\(a,b) (c,d) -> compareHands2 a c) (parseInput2 input)

parseInput2 :: String -> [([Int], Int)]
parseInput2 input = map (convertToHandAndBid . words) $ lines input
  where 
    convertToHandAndBid :: [String] -> ([Int], Int)
    convertToHandAndBid [h,b] = (map getVal $ chunksOf 1 h, read b)
    convertToHandAndBid _ = error "empty hand"
    getVal :: String -> Int
    getVal s = case s of
                "T" -> 10
                "J" -> 1
                "Q" -> 12
                "K" -> 13
                "A" -> 14
                _ -> read s

compareHands :: [Int] -> [Int] -> Ordering
compareHands hand1 hand2 = case compare (getType hand1) (getType hand2) of
                              EQ -> compareHandsSameType hand1 hand2
                              c -> c

compareHandsSameType :: [Int] -> [Int] -> Ordering
compareHandsSameType (a:as) (b:bs) = case (compare a b) of
                                      EQ -> compareHandsSameType as bs
                                      c -> c
compareHandsSameType _ _ = EQ

testHand1 :: [Int]
testHand1 = [1,1,1,1,1]

testHand2 :: [Int]
testHand2 = [1,1,1,14,14]

testHand3 :: [Int]
testHand3 = [2,2,3,3,11]

getType :: [Int] -> Int
getType [] = error "empty hand"
getType hand | length groupedHand == 1 = 7
             | length groupedHand == 2 && (checkLengths [1,4] groupedHand) = 6
             | length groupedHand == 2 && (checkLengths [2,3] groupedHand) = 5
             | length groupedHand == 3 && (checkLengths [1,1,3] groupedHand) = 4
             | length groupedHand == 3 && (checkLengths [1,2,2] groupedHand) = 3
             | length groupedHand == 4 = 2
             | length groupedHand == 5 = 1
             | otherwise = error "invalid hand"
  where groupedHand = sortBy (\a b -> compare (length a) (length b)) $ group $ sort hand
        firstGroup = head groupedHand

checkLengths :: [Int] -> [[Int]] -> Bool
checkLengths list hand = and $ map (\(a, b) -> length b == a) $ zip list hand

getType2 :: [Int] -> Int
getType2 [] = error "invalid hand"
getType2 hand | length groupedHand == 1 = 7
             | length groupedHand == 2 && (checkLengths [1,4] groupedHand) = 6
             | length groupedHand == 2 && (checkLengths [2,3] groupedHand) = 5
             | length groupedHand == 3 && (checkLengths [1,1,3] groupedHand) = 4
             | length groupedHand == 3 && (checkLengths [1,2,2] groupedHand) = 3
             | length groupedHand == 4 = 2
             | length groupedHand == 5 = 1
             | otherwise = error "invalid hand"
  where groupedHand = moveJokers (sortBy (\a b -> compare (length a) (length b)) $ group $ sort hand)

moveJokers :: [[Int]] -> [[Int]]
moveJokers list = take (length listWoJokers - 1) listWoJokers ++ [last listWoJokers ++ (concat $ filter (\x -> head x == 1) list)]
  where listWoJokers = filter (\x -> head x /= 1) list

compareHands2 :: [Int] -> [Int] -> Ordering
compareHands2 hand1 hand2 = case compare (getType2 hand1) (getType2 hand2) of
                              EQ -> compareHandsSameType hand1 hand2
                              c -> c