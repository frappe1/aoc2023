import Data.List.Split (splitOn)
import Data.Maybe
import Data.List (transpose, sort, groupBy)

data Rock = Empty | Round | Square deriving (Eq, Ord, Show)
data Dir = North | West | South | East deriving (Eq, Ord, Show)

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
  print $ move East $ parseInput contents

part1solution :: String -> Int
part1solution input = calculateWeight $ move North (parseInput input)

part2solution :: String -> Int
part2solution input = calculateWeight $ moveMultipleTimes (take (1000000000*4) $ cycle [North, West, South, East]) (parseInput input)

-- Parses into vertical lines
parseInput :: String -> [[Rock]]
parseInput input = map (map toRock) $ transpose $ lines input
  where toRock c = case c of
                    '.' -> Empty
                    'O' -> Round
                    '#' -> Square
                    _ -> error "invalid rock"

moveMultipleTimes :: [([Rock], [Rock])] -> [Dir] -> [[Rock]] -> [[Rock]]
moveMultipleTimes _ [] rocks = rocks
moveMultipleTimes dp (d:ds) rocks = moveMultipleTimes newDp ds newMap
  where (newDp, newMap) = move dp d rocks

move :: [([Rock], [Rock])] -> Dir -> [[Rock]] -> ([([Rock], [Rock])], [[Rock]])
move dp dir rocks | dir == North || dir == South = (newDpEntriesNS ++ dp, newMapNS)
                  | dir == East || dir == West = undefined--transpose $ map (concat . sortCol dir . groupBy (\a b -> notElem Square [a,b])) (transpose rocks)
                    where 
                      moveColNS :: [Rock] -> (Maybe ([Rock], [Rock]), [Rock])
                      moveColNS col | length (findInDp col) > 0 = (Nothing, head $ findInDp col)
                                  | otherwise = (Just (col, (concat . sortCol dir . groupBy (\a b -> notElem Square [a,b])) col), (concat . sortCol dir . groupBy (\a b -> notElem Square [a,b])) col)
                      findInDp col = [after | (before, after) <- dp, before == col]
                      rawNS = (map moveColNS rocks)
                      newDpEntriesNS = map fromJust $ filter (isJust) $ map (fst) raw
                      newMapNS = map snd rawNS
--move :: Dir -> [Rock] -> [Rock]
--move dir col | dir == North || dir == South = concat $ sortCol dir $ groupBy (\a b -> notElem Square [a,b]) col
--              | dir == East || dir == West = undefined
--move _ _ = undefined

sortCol :: Dir -> [[Rock]] -> [[Rock]]
sortCol dir (c:cs) | all (==Square) c = c : sortCol dir cs
                   | dir == North || dir == East = (reverse $ sort c) : sortCol dir cs
                   | otherwise = (sort c) : sortCol dir cs
sortCol _ _ = []

calculateWeight :: [[Rock]] -> Int
calculateWeight rocks = sum $ [sum $ zipWith rockScore (reverse col) [1..] | col <- rocks]
  where rockScore Round s = s
        rockScore _ _ = 0