import Data.List (sortOn)
import Data.List.Split (splitOn)

data SeedRange = SeedRange {
    start :: Int,
    seedCount :: Int
} deriving (Show)

data MapRange = Range {
    source :: Int,
    destination :: Int,
    length :: Int
} deriving (Show)

parseSeeds :: String -> [SeedRange]
parseSeeds seedsString =
    createRange $ map read $ drop 1 (words seedsString)
    where
        createRange [] = []
        createRange (s:c:rest) = SeedRange s c : createRange rest

parseMap :: String -> [MapRange]
parseMap mapString =
    sortOn source $ map createRange $ drop 1 $ lines mapString
    where
        createRange line =
            let [destination, source, length] = map read $ words line
            in Range source destination length

translate :: Int -> [MapRange] -> Int
translate number [] = number
translate number (range:rs) =
    if number >= source range && number < source range + Main.length range then
        destination range + number - source range
    else
        translate number rs

createSeedRange :: Int -> Int -> SeedRange
createSeedRange first last = SeedRange first (last - first + 1)

translateSeedRange :: MapRange -> Int -> Int -> SeedRange
translateSeedRange mapRange first last =
    SeedRange (first + adjustment) (last - first + 1)
    where
        adjustment = destination mapRange - source mapRange

getMappedRanges :: SeedRange -> [MapRange] -> [SeedRange]
getMappedRanges seedRange [] = [seedRange]
getMappedRanges seedRange (mapRange:ms) =
    if inMapRange then
        beforeRange ++ insideRange ++ processRemaining
    else
        getMappedRanges seedRange ms
    where
        firstSeed = start seedRange
        lastSeed = firstSeed + seedCount seedRange - 1
        firstMap = source mapRange
        lastMap = firstMap + Main.length mapRange - 1
        inMapRange =
            firstSeed >= firstMap && firstSeed <= lastMap ||
            lastSeed >= firstMap && lastSeed <= lastMap ||
            firstSeed < firstMap && lastSeed > lastMap
        beforeRange =
            if firstSeed < firstMap then
                [createSeedRange firstSeed (min lastSeed (firstMap - 1))]
            else
                []
        insideRange =
            if lastSeed >= firstMap then
                [translateSeedRange mapRange (max firstSeed firstMap) (min lastSeed lastMap)]
            else
                []
        processRemaining =
            if lastSeed > lastMap then
                getMappedRanges (SeedRange (lastMap + 1) (lastSeed - lastMap)) ms
            else
                []

findAllRanges :: SeedRange -> [[MapRange]] -> [SeedRange]
findAllRanges seedRange [] = [seedRange]
findAllRanges seedRange (mapRange:ms) =
    concatMap (`findAllRanges` ms) ranges
    where
        ranges = getMappedRanges seedRange mapRange

main :: IO ()
main = do
    contents <- getContents
    let instructions = splitOn "\n\n" contents
    let seeds = parseSeeds $ head instructions
    let maps = map parseMap $ drop 1 instructions
    putStrLn "Input:"
    print seeds

    let locations = concatMap (`findAllRanges` maps) seeds
    putStrLn "\nLocations:"
    print locations

    putStrLn "\nMinimum location:"
    print $ minimum $ map start locations
