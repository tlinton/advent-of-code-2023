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

main :: IO ()
main = do
    contents <- getContents
    let cardStrings = map (drop 1 . splitOneOf ":|") $ lines contents
    let cards = map parseCard cardStrings
    let winningNumbers = map getWinningNumbers cards
    mapM_ (print . getWinningNumbers) cards
    let lengths = map length winningNumbers
    let values = map ((2 ^) . subtract 1) $ filter (> 0) lengths
    print $ sum values

