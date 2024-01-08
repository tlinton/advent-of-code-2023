import Data.List.Split (splitOn, splitOneOf)

data Cubes = Cubes {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Show)

updateCubes :: Cubes -> String -> Cubes
updateCubes (Cubes r g b) description
    | color == "red" = Cubes value g b
    | color == "green" = Cubes r value b
    | color == "blue" = Cubes r g value
    | otherwise = Cubes r g b
    where
        color = last $ words description
        value = read $ head $ words description

createSet :: String -> Cubes
createSet description =
    foldl updateCubes (Cubes 0 0 0) sets
    where
        sets = splitOn "," description


readGame :: String -> (Int, [Cubes])
readGame description =
    let
        (game:sets) = splitOneOf ":;" description
        gameNr = read $ last $ words game
        cubeElements = map createSet sets
    in
        (gameNr, cubeElements)

possibleGame :: (a, [Cubes]) -> Bool
possibleGame (_, cubes) = all possible cubes
    where possible cubes = red cubes <= 12 && green cubes <= 13 && blue cubes <= 14

main :: IO ()
main = do
    contents <- getContents
    let games = map readGame (lines contents)
    putStrLn "All Games:"
    mapM_ (\(nr, cubes) -> putStrLn $ show nr ++ ": " ++ show cubes) games
    let possible = filter possibleGame games
    putStrLn "Possible Games:"
    mapM_ (\(nr, cubes) -> putStrLn $ show nr ++ ": " ++ show cubes) possible
    print $ sum $ map fst possible
