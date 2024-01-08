import Data.Char (isNumber)

createValue :: (Num a, Read a) => String -> a
createValue line =
    read (head line : [last line])

main :: IO ()
main = do
     contents <- getContents
     let digits = map (filter isNumber) (lines contents)
     print $ sum $ map createValue digits
