import Data.List.Split (splitOn, splitOneOf)

parseCard :: [String] -> ([Int], [Int])
parseCard input =
    createTuple $ map parseList input
    where
        parseList = map read . words
        createTuple (a:b:_) = (a, b)

getWinningNumbers :: ([Int], [Int]) -> [Int]
getWinningNumbers (winning, have) =
    filter (`elem` have) winning

increaseCardCount :: Int -> Int -> [Int] -> [Int]
increaseCardCount 0 _ cs = cs
increaseCardCount number increment (cardCount:cs) =
    cardCount + increment : increaseCardCount (number - 1) increment cs

calculateCardCount :: [Int] -> [Int] -> [Int]
calculateCardCount _ [] = []
calculateCardCount (winCount:ws) (cardCount:cs) =
    cardCount : calculateCardCount ws newCardCount
    where
        newCardCount = increaseCardCount winCount cardCount cs

main :: IO ()
main = do
    contents <- getContents
    let cardStrings = map (drop 1 . splitOneOf ":|") $ lines contents
    let cards = map parseCard cardStrings
    let winningNumbers = map getWinningNumbers cards
    mapM_ (print . getWinningNumbers) cards
    let winCount = map length winningNumbers
    let initialCardCount =  replicate (length cards) 1
    let cardCount = calculateCardCount winCount initialCardCount
    print $ sum cardCount
