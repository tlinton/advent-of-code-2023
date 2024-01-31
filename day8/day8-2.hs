import Data.List (transpose)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!))
import Text.Regex (mkRegex, matchRegex)

data Node = Node {
    name :: String,
    left :: String,
    right :: String
} deriving (Eq, Show)

parseNodes :: Map.Map String Node -> [String] -> Map.Map String Node
parseNodes nodeData [] = nodeData
parseNodes nodeData (current:rest) =
    case matchRegex regexp current of
        Just [name, left, right] -> parseNodes (updateData $ Node name left right) rest
        Nothing -> parseNodes nodeData rest
    where
        regexp = mkRegex "([[:alnum:]]{3}) *= *\\(([[:alnum:]]{3}), *([[:alnum:]]{3})\\)"
        updateData newNode = Map.insert (name newNode) newNode nodeData

parse :: [String] -> (String, Map.Map String Node)
parse instructions = (steps, nodes)
    where
        steps = head instructions
        nodes = parseNodes Map.empty $ drop 2 instructions

findPath :: Map.Map String Node -> [Char] -> Node -> [String]
findPath nodeData (currentStep:steps) currentNode =
    name currentNode : findPath nodeData steps (nodeData ! nextNodeName)
    where
        nextNodeName
            | currentStep == 'L' = left currentNode
            | currentStep == 'R' = right currentNode

isAtEnd :: [String] -> Bool
isAtEnd = all (\node -> last node == 'Z')

findEnd :: [[String]] -> [[String]]
findEnd [] = []
findEnd [current] = [current]
findEnd (current:rest) =
    if isAtEnd current then
        [current]
    else
        current : findEnd rest

countEndSteps :: [String] -> Int -> [Int]
countEndSteps [] _ = []
countEndSteps (node:ns) stepCount =
    if last node == 'Z' then
        stepCount : countEndSteps ns 1
    else
        countEndSteps ns (stepCount + 1)

main :: IO ()
main = do
    contents <- getContents
    let (steps, nodes) = parse $ lines contents
    let startNodes = filter (\node -> last (name node) == 'A') $ Map.elems nodes
    putStrLn $ "Start nodes: " ++ show startNodes
    let paths = map (findPath nodes (cycle steps)) startNodes
    print $ length $ findEnd $ transpose (take 2 paths)
    -- This only works if the first path to the end is the same length as the
    -- rest, i.e. that the bath from xxA to xxZ is the same as from xxZ back to
    -- xxZ.
    mapM_ (print . take 10 . flip countEndSteps 0) paths
    putStrLn "Number of steps:"
    print $ foldl1 lcm $ map (head . flip countEndSteps 0) paths
