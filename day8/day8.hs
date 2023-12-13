import Prelude hiding (Right, Left)
import Data.List.Split
import GhcPlugins (Plugin(parsedResultAction))

data PreparseNode = PreparseNode String String String
data Node = Node String Int Int
data Dir = Left | Right

instance Show (Node) where
  show (Node id child1 child2) = show id ++ "-" ++ show child1 ++ "-" ++ show child2

instance Show Dir where
  show Left = "L"
  show Right = "R"

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
  print $ snd $ parseInput contents

part1solution :: String -> Int
part1solution input = iterateThroughMap (snd $ parseInput input) (cycle (fst $ parseInput input)) (findWithId "AAA" $ snd $ parseInput input)

part2solution :: String -> Int
part2solution input = fst combinedThreads + snd combinedThreads - 1
  where allNodes = snd $ parseInput input
        dirs = cycle $ fst $ parseInput input
        startNodes = filter (\(Node id c1 c2) -> last id == 'A') allNodes
        threads = map (findLoop allNodes dirs) startNodes
        combinedThreads = foldr combineThreads (head threads) (tail threads)


pathFinished :: [Node] -> Bool
pathFinished = all (\(Node id c1 c2) -> last id == 'Z')

preparseLine :: String -> [String]
preparseLine line = filter (\x -> x /="" && x /= "=") $ splitOneOf "(,) " line

parseInput :: String -> ([Dir], [Node])
parseInput input = (parseDir $ head $ lines input, map findChildrenIds $ preparsedNodes)
  where preparseNodes :: String -> PreparseNode
        preparseNodes line = PreparseNode (preparseLine line !! 0) (preparseLine line !! 1) (preparseLine line !! 2)
        findChildrenIds :: PreparseNode -> Node
        findChildrenIds (PreparseNode id childId1 childId2) = Node id (findIndexWithId childId1 preparsedNodes 0) (findIndexWithId childId2 preparsedNodes 0)

        preparsedNodes = map preparseNodes $ (drop 2 $ lines input)

findIndexWithId :: String -> [PreparseNode] -> Int -> Int
findIndexWithId id ((PreparseNode nodeId _ _):nodes) index | id == nodeId = index
                                                        | otherwise = findIndexWithId id nodes (index + 1)
findIndexWithId _ _ _ = error "node not found"

findIndex :: Node -> [Node] -> Int -> Int
findIndex node@(Node id _ _) ((Node id2 _ _):ns) index | id == id2 = index
                            | otherwise = findIndex node ns (index + 1)
findIndex _ _ _ = error "node not found"

parseDir :: String -> [Dir]
parseDir ('L':cs) = Left : parseDir cs
parseDir ('R':cs) = Right : parseDir cs
parseDir _ = []

findWithId :: String -> [Node] -> Node
findWithId id (node@(Node currentId _ _):nodes) | currentId == id = node
                                                | otherwise = findWithId id nodes
findWithId _ _ = error "no node found"

iterateThroughMap :: [Node] -> [Dir] -> Node -> Int
iterateThroughMap _ _ (Node "ZZZ" _ _) = 0
iterateThroughMap allNodes (d:ds) node@(Node id l r) = 1 + iterateThroughMap allNodes ds (allNodes !! step node d)
iterateThroughMap _ (d:ds) node = error ("weird node: " ++ show node)
iterateThroughMap _ _ _ = error "ran out of directions"

step :: Node -> Dir -> Int
step (Node id l r) Left = l
step (Node id l r) Right = r
step _ _ = error "empty node"

-- PART 2 ----
iterateUntilZ:: [Node] -> [Dir] -> Node -> [Node]
iterateUntilZ allNodes (d:ds) node@(Node id l r) | last id == 'Z' = [node]
                                                 | otherwise = (node : (iterateUntilZ allNodes ds (allNodes !! step node d)))
iterateUntilZ  _ _ _ = error "no directions"

findLoop :: [Node] -> [Dir] -> Node -> (Int, Int)
findLoop allNodes ds node = (length firstIt - loopLength, loopLength)
  where firstIt = iterateUntilZ allNodes ds node
        remainingDirs = drop (length firstIt - 1) ds
        secondIt = iterateUntilZ allNodes (drop 1 remainingDirs) (allNodes !! step (last firstIt) (head remainingDirs))
        loopLength = length secondIt

combineThreads :: (Int, Int) -> (Int, Int) -> (Int, Int)
combineThreads t1@(loopStart1, loopLength1) t2@(loopStart2, loopLength2) = (1, newLoopLength)
  where 
    newLoopLength = lcm loopLength1 loopLength2
    firstZAlign = loopStart2 + (findN t1 t2 0) * loopLength2 - newLoopLength

-- Finds n2
findN :: (Int, Int) -> (Int, Int) -> Int -> Int
findN (loopStart1, loopLength1) (loopStart2, loopLength2) n | mod (loopLength2 + (loopStart2 - loopStart1)) loopLength1 == 0 = div (loopLength2 + (loopStart2 - loopStart1)) loopLength1
                                                            | otherwise = findN (loopStart1, loopLength1) (loopStart2, loopLength2) (n+1)