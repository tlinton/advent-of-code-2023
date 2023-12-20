import Data.List.Split (splitOn)
import Data.Type.Coercion (trans)

data Range = Range {
    source :: Int,
    destination :: Int,
    length :: Int
} deriving (Show)

parseSeeds :: String -> [Int]
parseSeeds seedsString =
    map read $ drop 1 (words seedsString)

parseMap :: String -> [Range]
parseMap mapString =
    map createRange $ drop 1 $ lines mapString
    where
        createRange line =
            let [destination, source, length] = map read $ words line
            in Range source destination length

translate :: Int -> [Range] -> Int
translate number [] = number
translate number (range:rs) =
    if number >= source range && number < source range + Main.length range then
        destination range + number - source range
    else
        translate number rs

translateSeed :: Int -> [[Range]] -> Int
translateSeed = foldl translate

main :: IO ()
main = do
    contents <- getContents
    let instructions = splitOn "\n\n" contents
    print instructions
    let seeds = parseSeeds $ head instructions
    let maps = map parseMap $ drop 1 instructions
    print seeds
    print maps
    let translated = map (`translateSeed` maps) seeds
    print $ minimum translated
