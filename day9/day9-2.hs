diffs :: Num a => [a] -> [a]
diffs [] = []
diffs [a] = []
diffs (a:b:rest) = (b - a) : diffs (b:rest)

allDiffs :: [Int] -> [[Int]]
allDiffs line =
    currentDiff : nextDiff
    where
        currentDiff = diffs line
        nextDiff
            | all (== 0) currentDiff = []
            | otherwise = allDiffs currentDiff

main :: IO ()
main = do
    contents <- getContents
    let inputValues = map (map read . words) $ lines contents
    let sequences = map (\val -> val : allDiffs val) inputValues
    let firstValues = map (map head) sequences
    let previousValues = map (foldr (-) 0) firstValues
    putStrLn $ "Previous values: " ++ (show previousValues)
    putStrLn $ "Sum of previous values: " ++ (show $ sum previousValues)
