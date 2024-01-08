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

cubesNeeded :: (a, [Cubes]) -> (a, Cubes)
cubesNeeded (a, cubes) = (a, foldl needed (Cubes 0 0 0) cubes)
    where needed (Cubes r1 g1 b1) (Cubes r2 g2 b2) = Cubes (max r1 r2) (max g1 g2) (max b1 b2)

cubePower :: Cubes -> Int
cubePower (Cubes r g b) = r * g * b

main :: IO ()
main = do
    contents <- getContents
    let games = map readGame (lines contents)
    putStrLn "All Games:"
    mapM_ (\(nr, cubes) -> putStrLn $ show nr ++ ": " ++ show cubes) games
    let needed = map cubesNeeded games
    putStrLn "Needed cubes:"
    mapM_ (\(nr, cubes) -> putStrLn $ show nr ++ ": " ++ show cubes) needed
    let powers = map (cubePower . snd) needed
    print $ sum powers
