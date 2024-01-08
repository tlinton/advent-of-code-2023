import Data.Char (isDigit)
import qualified Data.Map.Strict as Map

data Number = Number {
    value :: Int,
    startX :: Int
} deriving (Show)

data State a = InNumber a | Waiting deriving(Show)

isGear :: Char -> Bool
isGear char = char == '*'

addGear :: Map.Map (Int, Int) [Int] -> (Int, Int, Char) -> Map.Map (Int, Int) [Int]
addGear gears (row, col, char)
    | isGear char = Map.insert (row, col) [] gears
    | otherwise = gears

findPotentialGears :: Map.Map (Int, Int) [Int] -> (Int, [Char]) -> Map.Map (Int, Int) [Int]
findPotentialGears gears (rowNr, input) =
    foldl addGear gears $ zip3 (repeat rowNr) [0..] input

updateGears :: Map.Map (Int, Int) [Int] -> Int -> Int -> Number -> Map.Map (Int, Int) [Int]
updateGears gears rowNr endX number =
    foldl updateMap gears [(y, x) | x <- [startX number - 1 .. endX], y <- [rowNr - 1 .. rowNr + 1]]
    where
        updateMap gears key = Map.adjust (value number :) key gears

scanRow ::
    Int
    -> (State Number, Map.Map (Int, Int) [Int])
    -> (Int, Char)
    -> (State Number, Map.Map (Int, Int) [Int])
scanRow rowNr (state, gears) (colNr, char) =
    case state of
        Waiting ->
            if isDigit char then
                (InNumber (Number (read [char]) colNr), gears)
            else
                (Waiting, gears)
        InNumber number ->
            if isDigit char then
                (InNumber (Number (value number * 10 + read [char]) (startX number)), gears)
            else
                (Waiting, updateGears gears rowNr colNr number)

mapPartNumbers :: Map.Map (Int, Int) [Int] -> (Int, [Char]) -> Map.Map (Int, Int) [Int]
mapPartNumbers symbols (rowNr, rowData) =
    snd $ foldl scanFunction (Waiting, symbols) (zip [0..] (rowData ++ "."))
    where
        scanFunction = scanRow rowNr

main :: IO ()
main = do
    contents <- getContents
    let potentialGears = foldl findPotentialGears Map.empty $ zip [0..] (lines contents)
    let potentialPartNumbers = foldl mapPartNumbers potentialGears $ zip [0..] (lines contents)
    let partNumbers = filter ((== 2) . length) $ Map.foldl (flip(:)) [] potentialPartNumbers
    putStrLn  "Part numbers:"
    mapM_ print partNumbers
    putStrLn "Sum of gear ratios:"
    print $ sum $ map product partNumbers
