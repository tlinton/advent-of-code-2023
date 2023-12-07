import Data.List.Split (splitOneOf)
import Debug.Trace (trace)

data Cubes = Cubes {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Show)

updateCubes :: Cubes -> String -> Cubes
updateCubes (Cubes r g b) description
    | color == "red" = Cubes (r + value) g b
    | color == "green" = Cubes r (g + value) b
    | color == "blue" = Cubes r g (b + value)
    | otherwise = Cubes r g b
    where
        color = last $ words description
        value = read $ head $ words description

readGame :: String -> (Int, Cubes)
readGame description =
    let
        (game:cubes) = splitOneOf ":;," description
        gameNr = read $ last $ words game
        cubeElements = foldl updateCubes (Cubes 0 0 0) cubes
    in
        (gameNr, cubeElements)

possibleGame :: (a, Cubes) -> Bool
possibleGame (_, cubes) = red cubes <= 12 && green cubes <= 13 && blue cubes <= 14

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
