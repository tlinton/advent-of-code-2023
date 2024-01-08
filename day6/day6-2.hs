parseInput :: [String] -> (Int, Int)
parseInput (t:d:_) =
    (duration, distance)
    where
        duration = read (concat $ drop 1 (words t))
        distance = read (concat $ drop 1 (words d))

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
    let (raceDuration, recordDistance) = parseInput $ lines contents
    let possibleDistances = map getPossibleDistances [raceDuration]
    let possibleWins = filterDistances possibleDistances [recordDistance]
    let winCount = map length possibleWins
    print winCount
