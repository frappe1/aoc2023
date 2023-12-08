import Prelude hiding (Right, Left)
import Data.List.Split
import GhcPlugins (Plugin(parsedResultAction))

--data Node = Empty | Node String (Node) (Node) | PlaceHolder String
data Node = Node String String String
data Dir = Left | Right

instance Show (Node) where
  --show (Node id child1 child2) = show id ++ "-" ++ show (getId child1) ++ "-" ++ show (getId child2)
  --show (PlaceHolder s) = "Placeholder: " ++ s
  --show _ = "empty node"
  show (Node id child1 child2) = show id ++ "-" ++ show child1 ++ "-" ++ show child2

instance Show Dir where
  show Left = "L"
  show Right = "R"

part1 :: IO()
part1 = do
  contents <- readFile "input.txt"
  print $ part1solution contents

test :: IO()
test = do
  contents <- readFile "input.txt"
  print $ snd $ parseInput contents

part1solution :: String -> Int
part1solution input = iterateThroughMap (snd $ parseInput input) (cycle (fst $ parseInput input)) (findWithId "AAA" $ snd $ parseInput input)

--testNode :: Node
--testNode = Node "AAA" (Node "BBB" Empty Empty) (Node "CCC" Empty Empty)

preparseLine :: String -> [String]
preparseLine line = filter (\x -> x /="" && x /= "=") $ splitOneOf "(,) " line

--parseInput :: String -> ([Dir], [Node])
--parseInput input = (parseDir $ head $ lines input, map (findChildren preparsedNodes) preparsedNodes)
--  where 
--    preparsedNodes = map convertToPlaceHolder (drop 2 $ lines input)
--    convertToPlaceHolder :: String -> Node
--    convertToPlaceHolder line = Node ((preparseLine line) !! 0) (PlaceHolder ((preparseLine line) !! 1)) (PlaceHolder ((preparseLine line) !! 2))

parseInput :: String -> ([Dir], [Node])
parseInput input = (parseDir $ head $ lines input, map convertToNode (drop 2 $ lines input))
  where convertToNode :: String -> Node
        convertToNode line = Node (preparseLine line !! 0) (preparseLine line !! 1) (preparseLine line !! 2)

parseDir :: String -> [Dir]
parseDir ('L':cs) = Left : parseDir cs
parseDir ('R':cs) = Right : parseDir cs
parseDir _ = []

--findChildren :: [Node] -> Node -> Node
--findChildren allNodes (Node id (PlaceHolder child1) (PlaceHolder child2)) = Node id (head [n | n <- allNodes, getId n == child1]) (head [n | n <- allNodes, getId n == child2])

getId :: Node -> String
getId (Node id _ _) = id
--getId (PlaceHolder id) = "Placeholder " ++ id
--getId _ = "no id"

findWithId :: String -> [Node] -> Node
findWithId id allNodes = head [n | n <- allNodes, getId n == id]

iterateThroughMap :: [Node] -> [Dir] -> Node -> Int
iterateThroughMap _ _ (Node "ZZZ" _ _) = 0
iterateThroughMap allNodes (d:ds) node@(Node id l r) = 1 + iterateThroughMap allNodes ds (findWithId (step node d) allNodes)
iterateThroughMap _ (d:ds) node = error ("weird node: " ++ show node)
iterateThroughMap _ _ _ = error "ran out of directions"

step :: Node -> Dir -> String
step (Node id l r) Left = l
step (Node id l r) Right = r
step _ _ = error "empty node"