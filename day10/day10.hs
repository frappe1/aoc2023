import Data.Maybe
import Data.List.Split
import Data.List (group, groupBy, sortBy)
data Dir = North | South | West | East deriving Eq

instance Show Dir where
  show North = "North"
  show South = "South"
  show East = "East"
  show West = "West"


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
  --print $ map (\l -> ((fst $ head l, length l))) $ filter (\l -> (snd . head) l == 1) $ map (sortBy (\(a,b) (c,d) -> compare a c)) $ groupBy (\(a,b) (c,d) -> b == d) $ (goThroughPipe2 (parseInput contents) (findStartOfLoop (parseInput contents) pipeList))
  --print $ (goThroughPipe2 (parseInput contents) (findStartOfLoop (parseInput contents) pipeList))--convertToBoolMap (parseInput contents) (goThroughPipe2 (parseInput contents) (findStartOfLoop (parseInput contents) pipeList)) 0
  print $ contents
testMap = [[False,False,False,False,False,False,False,False,False,False,False],[False,True,True,True,True,True,True,True,True,True,False],[False,True,True,True,True,True,True,True,True,True,False],[False,True,True,False,False,False,False,False,True,True,False],[False,True,True,False,False,False,False,False,True,True,False],[False,True,True,True,True,False,True,True,True,True,False],[False,True,False,False,True,False,True,False,False,True,False],[False,True,True,True,True,False,True,True,True,True,False],[False,False,False,False,False,False,False,False,False,False,False]]
part1solution :: String -> Int
part1solution input = div ((goThroughPipe (parseInput input) (head (findStartOfLoop (parseInput input) pipeList)))+1) 2

part2solution :: String -> Int
part2solution input = fromJust $ head $ filter (isJust) $ map (\x -> calculateArea pipeMap $ goThroughPipe2 pipeMap (x )) (findStartOfLoop pipeMap pipeList)--length $ takeWhile (\x -> x == False) (drop ((1)+1) row)
  --calculateArea pipeMap $ goThroughPipe2 pipeMap (findStartOfLoop pipeMap pipeList)
  --createMap (goThroughPipe2 pipeMap (findStartOfLoop pipeMap pipeList)) (length $ head pipeMap, length pipeMap) 0
  where pipeMap = parseInput input
        --path = goThroughPipe2 pipeMap (findStartOfLoop pipeMap pipeList)
        --boolMap = convertToBoolMap pipeMap path 0
        --row = boolMap !! 2

parseInput :: String -> [[Char]]
parseInput input = lines input

goThroughPipe :: [[Char]] -> (Dir, Int, Int) -> Int
goThroughPipe pipeMap currentPos@(dir,x,y) | currentPipe == 'S' = 0
                                           | otherwise = 1 + goThroughPipe pipeMap (fromJust $ step pipeMap pipeList dir (x,y))
  where currentPipe = (pipeMap !! y) !! x

step :: [[Char]] -> [(Char, Dir, Dir)] -> Dir -> (Int, Int) -> Maybe (Dir, Int, Int)
step pipeMap pipes dir currentPos@(x, y) | not (correctCoordinate pipeMap currentPos) = error "incorrect coordinate"
                                         | not isPipe = Nothing
                                         | dir == (opposite pipeEnd1) = Just $ goInDir pipeEnd2 currentPos
                                         | dir == (opposite pipeEnd2) = Just $ goInDir pipeEnd1 currentPos
                                         | otherwise = Nothing
  where
    currentPipe = (pipeMap !! y) !! x
    (_, pipeEnd1, pipeEnd2) = head $ filter (\(c, _, _) -> c == currentPipe) pipes
    isPipe = case ((pipeMap !! y) !! x) of
                  'S' -> False
                  '.' -> False
                  _ -> True

correctCoordinate :: [[Char]] -> (Int,Int) -> Bool
correctCoordinate pipeMap (x,y) = x >= 0 && x < (length $ head pipeMap) && y >= 0 && y < length pipeMap

opposite :: Dir -> Dir
opposite East = West
opposite West = East
opposite South = North
opposite North = South

goInDir :: Dir -> (Int, Int) -> (Dir, Int, Int)
goInDir dir (x,y) = case dir of
                      North -> (North, x, y-1)
                      South -> (South, x, y+1)
                      East -> (East, x+1, y)
                      West -> (West, x-1, y)

pipeList :: [(Char, Dir, Dir)]
pipeList = [('|', North, South), ('-', East, West), ('L', North, East), ('J', North, West), ('F', South, East), ('7', South, West)]

findStartOfLoop :: [[Char]] -> [(Char, Dir, Dir)] -> [(Dir, Int, Int)]
findStartOfLoop pipeMap pipes =  (map (\(a,b,c,d) ->(a,b,c)) $ filter (\(_, _, _, b) -> b) testFirstMove)
  where sPos = findS pipeMap 0
        testFirstMove :: [(Dir, Int, Int, Bool)]
        testFirstMove = zipWith (findFirstPipe pipeMap pipes) [North, South, East, West] (repeat sPos)



findFirstPipe :: [[Char]] -> [(Char, Dir, Dir)] -> Dir -> (Int, Int) -> (Dir, Int, Int, Bool)
findFirstPipe pipeMap pipes dir (x,y) = (dirTest, xTest, yTest, canGoInPipe)
  where (dirTest, xTest, yTest) = goInDir dir (x,y)

        canGoInPipe = case step pipeMap pipes dirTest (xTest, yTest) of
                        Nothing -> False
                        Just _ -> True

findS :: [[Char]] -> Int -> (Int, Int)
findS (p:pipeMap) y | elem 'S' p = (findSX p 0, y)
                    | otherwise = findS pipeMap (y+ 1)
findS _ _ = error "sus"

findSX :: [Char] -> Int -> Int
findSX (r:row) x | r == 'S' = x
                 | otherwise = findSX row (x+1)
findSX _ _ = error "sus"

--Part 2 ---
{-goThroughPipe2 :: [[Char]] -> (Dir, Int, Int) -> [(Int, Int)]
goThroughPipe2 pipeMap currentPos@(dir,x,y) | currentPipe == 'S' = [(x,y)]
                                           | otherwise = (x,y) : (goThroughPipe2 pipeMap (fromJust $ step pipeMap pipeList dir (x,y)))
  where currentPipe = (pipeMap !! y) !! x

createMap :: [(Int, Int)] -> (Int, Int) -> Int -> [[Bool]]
createMap pipePoses mapSize@(xm, ym) acc | acc == ym = []
                                         | otherwise = createRow (findXPoses acc $ (pipePoses)) xm 0 : createMap pipePoses mapSize (acc+1)

findXPoses :: Int -> [(Int, Int)] -> [(Int, Int)]
findXPoses yPos poses = extractRelevantPoses yPos $ groupBy (\(a,b) (c,d) -> b == d) poses

extractRelevantPoses :: Int -> [[(Int, Int)]] -> [(Int, Int)]
extractRelevantPoses yPos poses = map (\l -> ((fst $ head l, length l))) $ filter (\l -> (snd . head) l == yPos) poses

createRow :: [(Int, Int)] -> Int -> Int -> [Bool]
createRow ((s, l):xPoses) size acc | acc == size = []
                          | s == acc = True : createRow xPoses size (acc+1)
                          | otherwise = False : createRow ((s,l):xPoses) size (acc+1)
createRow _ _ _ = []

calculateArea :: [[Bool]] -> [Int]
calculateArea pipeMap = map (calculateAreaRow . groupTrue) pipeMap

calculateAreaRow :: [[Bool]] -> Int
calculateAreaRow (a:b:c:cy) | head a == True && head b == True = calculateAreaRow (c:cy)
                            | head a == True && head b == False = length b + calculateAreaRow cy
                            | otherwise = calculateAreaRow (b:c:cy)
calculateAreaRow _ = 0

groupTrue :: [Bool] -> [[Bool]]
groupTrue allBools = groupTrueHelper $ group allBools

groupTrueHelper :: [[Bool]] -> [[Bool]]
groupTrueHelper (b:bs) | head b == False = b : (groupTrueHelper bs)
                        | otherwise = (chunksOf 1 b) ++ (groupTrueHelper bs)
groupTrueHelper _ = []-}

goThroughPipe2 :: [[Char]] -> (Dir, Int, Int) -> [(Dir, Dir, Int, Int)]
goThroughPipe2 pipeMap currentPos@(dir,x,y) | currentPipe == 'S' = [(dir, East,x,y)]
                                           | otherwise = (dir, f $ fromJust $ step pipeMap pipeList dir (x,y), x,y) : goThroughPipe2 pipeMap (fromJust $ step pipeMap pipeList dir (x,y))
  where currentPipe = (pipeMap !! y) !! x
        f (a,b,c) = a

calculateArea :: [[Char]] -> [(Dir, Dir, Int, Int)] -> Maybe Int
calculateArea pipeMap path | all (isJust) $ map (calculateAreaRow (convertToBoolMap pipeMap path 0)) path = Just $ sum  $ map (fromJust . (calculateAreaRow  $ convertToBoolMap pipeMap path 0)) path 
                            | otherwise = Nothing--sum $ map (length . filter (==True)) $ foldr combineBoolMaps (replicate (length pipeMap) (replicate (length $ head pipeMap) False)) $ map (calculateAreaRow (convertToBoolMap pipeMap path 0)) path
  where combineBoolMaps a b = zipWith (zipWith f) a b
        f True _ = True
        f _ True = True
        f _ _ = False

calculateAreaRow :: [[Bool]] -> (Dir, Dir, Int, Int) -> Maybe Int
calculateAreaRow boolMap (dirIn, dirOut, pipeX, pipeY) | dirIn == South || dirOut == South = case (all (==False) (drop (pipeX+1) row)) of
                                                                                                False -> Just $ length $ takeWhile (\x -> x == False) (drop (pipeX+1) row)
                                                                                                _ -> Nothing --replicate pipeY emptyRow ++ [beforeArea ++ area ++ replicate (length row - (length beforeArea + length area)) False] ++ replicate (length boolMap - pipeY - 1) emptyRow
  where row = boolMap !! pipeY
        isPipe x = (boolMap !! pipeY) !! pipeX
        beforeArea = replicate (pipeX + 1) False
        area = replicate (length (takeWhile (\x -> x == False) (drop (pipeX+1) row))) True
        emptyRow = replicate (length row) False
calculateAreaRow boolMap (_,_,_,_) = Just 0--(replicate (length boolMap) (replicate (length $ head boolMap) False))

convertToBoolMap :: [[Char]] -> [(Dir, Dir, Int, Int)] -> Int -> [[Bool]]
convertToBoolMap pipeMap path acc | acc == maxY + 1 = []
                                  | otherwise = convertRow (pipeMap !! acc) (filter (\(_,_, _, y) -> y == acc) path) 0 : convertToBoolMap pipeMap path (acc+1)
  where maxY = length pipeMap - 1

convertRow :: [Char] -> [(Dir, Dir, Int, Int)] -> Int -> [Bool]
convertRow row path acc | acc == maxX + 1 = []
                        | otherwise = any (\(_,_, x, _) -> x == acc) path : convertRow row path (acc+1)
  where maxX = length row - 1

  