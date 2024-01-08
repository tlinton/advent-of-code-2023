import Data.Char (isDigit)
import qualified Data.Set as Set

data Number = Number {
    value :: Int,
    startX :: Int
} deriving (Show)

data State a = InNumber a | Waiting deriving(Show)

isSymbol :: Char -> Bool
isSymbol char = not (isDigit char) && (char /= '.')

addSymbol :: (Ord a, Ord b) => Set.Set (a, b) -> (a, b, Char) -> Set.Set (a, b)
addSymbol symbols (row, col, char)
    | isSymbol char = Set.insert (row, col) symbols
    | otherwise = symbols

findSymbols :: (Ord a, Ord b, Num b, Enum b) => Set.Set (a, b) -> (a, [Char]) -> Set.Set (a, b)
findSymbols symbols (rowNr, input) =
    foldl addSymbol symbols $ zip3 (repeat rowNr) [0..] input

isPartNumber :: Set.Set (Int, Int) -> Int -> Int -> Number -> Bool
isPartNumber symbols rowNr endX number =
    any (`Set.member` symbols) [(y, x) | x <- [startX number - 1 .. endX], y <- [rowNr - 1 .. rowNr + 1]]

scanRow :: Set.Set(Int, Int) -> Int -> (State Number, [Int]) -> (Int, Char) -> (State Number, [Int])
scanRow symbols rowNr (state, numbers) (colNr, char) =
    case state of
        Waiting ->
            if isDigit char then
                (InNumber (Number (read [char]) colNr), numbers)
            else
                (Waiting, numbers)
        InNumber number ->
            if isDigit char then
                (InNumber (Number (value number * 10 + read [char]) (startX number)), numbers)
            else if isPartNumber symbols rowNr colNr number then
                (Waiting, value number : numbers)
            else
                (Waiting, numbers)

findPartNumbers :: Set.Set (Int, Int) -> Int -> [Char] -> (State Number, [Int])
findPartNumbers symbols rowNr rowData =
    foldl scanFunction (Waiting, []) (zip [0..] (rowData ++ "."))
    where
        scanFunction = scanRow symbols rowNr

main :: IO ()
main = do
    contents <- getContents
    let symbols = foldl findSymbols Set.empty $ zip [0..] (lines contents)
    let partNumbers = map snd $ zipWith (findPartNumbers symbols) [0..] (lines contents)
    mapM_ print $ zip [1..] partNumbers
    print $ sum $ concat partNumbers
