parseInput :: [String] -> ([Int], [Int])
parseInput (t:d:_) =
    (durations, distances)
    where
        durations = map read $ drop 1 (words t)
        distances = map read $ drop 1 (words d)

getPossibleDistances :: Int -> [Int]
getPossibleDistances raceDuration =
    map getDistance [0 .. raceDuration]
    where
        getDistance chargeTime = (raceDuration - chargeTime) * chargeTime


filterDistances :: [[Int]] -> [Int] -> [[Int]]
filterDistances [] _ = []
filterDistances _ [] = []
filterDistances (possibleDistances:ps) (recordDistance:rs) =
    filter (> recordDistance) possibleDistances : filterDistances ps rs

main :: IO ()
main = do
    contents <- getContents
    let (raceDurations, recordDistances) = parseInput $ lines contents
    let possibleDistances = map getPossibleDistances raceDurations
    let possibleWins = filterDistances possibleDistances recordDistances
    let winCount = map length possibleWins
    print $ product winCount
