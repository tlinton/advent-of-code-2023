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
        regexp = mkRegex "([[:alpha:]]{3}) *= *\\(([[:alpha:]]{3}), *([[:alpha:]]{3})\\)"
        updateData newNode = Map.insert (name newNode) newNode nodeData

parse :: [String] -> (String, Map.Map String Node)
parse instructions = (steps, nodes)
    where
        steps = head instructions
        nodes = parseNodes Map.empty $ drop 2 instructions

findPath :: Map.Map String Node -> Node -> [Char] -> [String]
findPath nodeData currentNode (currentStep:steps)
    | name currentNode == "ZZZ" = ["ZZZ"]
    | currentStep == 'L' = name currentNode : findPath nodeData (nodeData ! left currentNode) steps
    | currentStep == 'R' = name currentNode : findPath nodeData (nodeData ! right currentNode) steps

main :: IO ()
main = do
    contents <- getContents
    let (steps, nodes) = parse $ lines contents
    putStrLn "Path:"
    let path = findPath nodes (nodes ! "AAA") $ cycle steps
    print path
    putStrLn $ "The path contains " ++ show (length path - 1) ++ " steps."
