import Data.Char (isNumber)
import Data.List (isPrefixOf)

createValue :: (Num a, Read a) => String -> a
createValue line =
    read (head line : [last line])

findNumbers :: String -> String
findNumbers [] = []
findNumbers (x:xs) =
    case getNumber (x:xs) of
        Just n -> n : findNumbers xs
        Nothing -> findNumbers xs

getNumber :: String -> Maybe Char
getNumber (x:xs)
    | isNumber x = Just x
    | "one" `isPrefixOf` (x:xs) = Just '1'
    | "two" `isPrefixOf` (x:xs) = Just '2'
    | "three" `isPrefixOf` (x:xs) = Just '3'
    | "four" `isPrefixOf` (x:xs) = Just '4'
    | "five" `isPrefixOf` (x:xs) = Just '5'
    | "six" `isPrefixOf` (x:xs) = Just '6'
    | "seven" `isPrefixOf` (x:xs) = Just '7'
    | "eight" `isPrefixOf` (x:xs) = Just '8'
    | "nine" `isPrefixOf` (x:xs) = Just '9'
    | otherwise = Nothing

main :: IO ()
main = do
     contents <- getContents
     let digits = map findNumbers (lines contents)
     print $ sum $ map createValue digits
