{-# LANGUAGE InstanceSigs #-}
import Data.List (group, sort, singleton)

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Show, Bounded, Enum)

instance Read Card where
    readsPrec _ (value:vs) =
        case value of
            '2' -> [(Two, vs)]
            '3' -> [(Three, vs)]
            '4' -> [(Four, vs)]
            '5' -> [(Five, vs)]
            '6' -> [(Six, vs)]
            '7' -> [(Seven, vs)]
            '8' -> [(Eight, vs)]
            '9' -> [(Nine, vs)]
            'T' -> [(Ten, vs)]
            'J' -> [(Jack, vs)]
            'Q' -> [(Queen, vs)]
            'K' -> [(King, vs)]
            'A' -> [(Ace, vs)]
            _ -> []

data Hand = Hand {
    cards :: [Card],
    bid :: Int,
    handType :: HandType
} deriving (Eq, Show)

instance Read Hand where
    readsPrec _ handString =
        [(Hand cards (read bidString) (createHandType cards), "") ]
        where
            [cardsString, bidString] = words handString
            cards = map (read . singleton) cardsString

instance Ord Hand where
    compare :: Hand -> Hand -> Ordering
    compare (Hand cards1 bid1 type1) (Hand cards2 bid2 type2)
        | type1 /= type2 = compare type1 type2
        | otherwise = compare cards1 cards2

data HandType = HighCard | OnePair | TwoPairs | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
    deriving (Eq, Ord, Show, Bounded, Enum)

createHandType :: [Card] -> HandType
createHandType cards
    | cardCounts == [5] = FiveOfAKind
    | cardCounts == [1, 4] = FourOfAKind
    | cardCounts == [2, 3] = FullHouse
    | cardCounts == [1, 1, 3] = ThreeOfAKind
    | cardCounts == [1, 2, 2] = TwoPairs
    | cardCounts == [1, 1, 1, 2] = OnePair
    | otherwise = HighCard
    where cardCounts = sort $ map length $ group $ sort cards

main :: IO ()
main = do
    contents <- getContents
    let hands :: [Hand]
        hands = map read $ lines contents
    mapM_ print $ zip [1..] $ sort hands
    print $ sum $ zipWith (\rank hand -> rank * bid hand)  [1..] $ sort hands
