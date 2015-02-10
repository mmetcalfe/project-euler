-- 
-- Problem 54: Poker hands
-- (Published on Friday, 10th October 2003, 06:00 pm; Solved by 18901)
-- 
--     In the card game poker, a hand consists of five cards and are
--  ranked, from lowest to highest, in the following way:
-- 
--     - *High Card*: Highest value card.
--     - *One Pair*: Two cards of the same value.
--     - *Two Pairs*: Two different pairs.
--     - *Three of a Kind*: Three cards of the same value.
--     - *Straight*: All cards are consecutive values.
--     - *Flush*: All cards of the same suit.
--     - *Full House*: Three of a kind and a pair.
--     - *Four of a Kind*: Four cards of the same value.
--     - *Straight Flush*: All cards are consecutive values of same
--        suit.
--     - *Royal Flush*: Ten, Jack, Queen, King, Ace, in same suit.
-- 
-- 
--     The cards are valued in the order:
--  2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
-- 
--     If two players have the same ranked hands then the rank made up
--  of the highest value wins; for example, a pair of eights beats a
--  pair of fives (see example 1 below). But if two ranks tie, for
--  example, both players have a pair of queens, then highest cards in
--  each hand are compared (see example 4 below); if the highest cards
--  tie then the next highest cards are compared, and so on.
-- 
--     Consider the following five hands dealt to two players:
-- 
--          *Hand*      *Player 1*          *Player 2*            *Winner*
--           *1*      5H 5C 6S 7S KD       2C 3S 8S 8D TD         Player 2
--                    Pair of Fives        Pair of Eights
--           
--           *2*      5D 8C 9S JS AC       2C 5C 7D 8S QH         Player 1
--                    Highest card Ac      Highest card Queen  
--           
--           *3*      2D 9C AS AH AC       3D 6D 7D TD QD         Player 2
--                    Three Aces           Flush  with Diamonds 
--            
--           *4*      4D 6S 9H QH QC       3D 6D 7H QD QS         Player 1
--                    Pair of Queens       Pair of Queens
--                    Highest card Nine    Highest card Seven
--
--           *5*      2H 2D 4C 4D 4S       3C 3D 3S 9S 9D         Player 1
--                    Full House           Full House
--                    With Three Fours     With Three Threes
--
--     The file, [poker.txt](project/resources/p054_poker.txt),
--  contains one-thousand random hands dealt to two players. Each line
--  of the file contains ten cards (separated by a single space): the
--  first five are Player 1's cards and the last five are Player 2's
--  cards. You can assume that all hands are valid (no invalid
--  characters or repeated cards), each player's hand is in no specific
--  order, and in each hand there is a clear winner.
-- 
--     How many hands does Player 1 win?

import Data.List
import Data.Function (on)
import Data.Char
import Data.Ord
import System.IO

data Suit = H | D | C | S
            deriving (Eq, Ord, Show, Read)

data Rank = A | K | Q | J | T | N Integer
            deriving (Eq, Ord, Show, Read)

data SymCard = SymCard Rank Suit
            deriving (Eq, Ord, Show, Read)

type Card = (Integer, Suit)

data Hand = Hand [Card]
            deriving (Eq, Show)

instance Ord Hand where
    compare a b = EQ

data HandType = Unknown
              | StraightFlush
              | FourOfAKind
              | FullHouse
              | Flush
              | Straight
              | ThreeOfAKind
              | TwoPairs
              | OnePair
              | HighCard
              deriving (Eq, Ord, Show)

type Score = (HandType, [Integer])
           -- | StraightFlush Card
           -- | FourOfAKind Integer
           -- | FullHouse Integer
           -- | Flush Suit
           -- | Straight Integer
           -- | ThreeOfAKind Integer
           -- | TwoPairs [Integer]
           -- | OnePair Integer
           -- | HighCard Integer
            -- deriving (Eq, Show, Read)

scoreHand (Hand cards)
    -- | isStraightFlush = (StraightFlush, [last cards])
    | isFourOfAKind   = (FourOfAKind, rankGroups)
    -- | isFullHouse     = (FullHouse, [fst . fst . head $ head rankGroups])
    -- | isFlush         = (Flush, reverse ranks)
    -- | isStraight      = (Straight, [highCardRank])
    -- | isThreeOfAKind  = (ThreeOfAKind, [fst . head $ head rankGroups])
    -- | isTwoPairs      = (TwoPairs, (sort $ map (fst . head) (take 2 rankGroups)))
    -- | isOnePair       = (OnePair, [fst . head $ head rankGroups])
    -- | isHighCard      = (HighCard, reverse ranks)
    | otherwise       = (Unknown, [])
    where compareLength = compare `on` length
          sortByMap f = sortBy (compare `on` f)
          groupByMap f = groupBy ((==) `on` f)
          suits = reverse . sortByMap length . groupByMap snd . sortByMap snd $ cards
          rankGroups = reverse . sortByMap length . groupByMap fst . sortByMap fst $ cards
          ranks = sort $ map fst cards
          isFourOfAKind = length (head rankGroups) == 4
          isFullHouse = (length (head rankGroups) == 3) && (length rankGroups == 2)
          isFlush = length suits == 1
          isStraight = tail (zipWith (-) ranks (0 : ranks)) == [1, 1, 1, 1]
          isStraightFlush = isStraight && isFlush
          isThreeOfAKind  = length (head rankGroups) == 3
          isTwoPairs      = all ((==2) . length) (take 2 rankGroups)
          isOnePair       = length (head rankGroups) == 2
          isHighCard      = True
          highCardRank = last ranks

integerRank r = case r of
                    (N i) -> i
                    T     -> 10
                    J     -> 11
                    Q     -> 12
                    K     -> 13
                    A     -> 14

toCard (SymCard r s) = (integerRank r, s)

-- handsStr = "8C TS KC 9H 4S 7D 2S 5D 3S AC"

-- hands = (read . words) handsStr :: [SymCard]

toString :: SymCard -> String
toString (SymCard r s) =
    let rs = case r of
                (N i) -> show i
                _     -> show r
    in rs ++ show s

fromString :: String -> SymCard
fromString [rt, st] =
    let d = ord rt
        r = if d >= ord 'A' && d <= ord 'Z'
                then read [rt] :: Rank
                else read ("N " ++ [rt]) :: Rank 
        s = read [st] :: Suit
    in SymCard r s

lineToHands :: String -> (Hand, Hand)
lineToHands str =
    let cards = map (toCard . fromString) $ words str
        (h1, h2) = splitAt 5 cards
    in (Hand (sort h1), Hand (sort h2))

scoreHands (a, b) = (scoreHand a, scoreHand b)

main =
    withFile "problem_pages/project/resources/p054_poker.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        let hands = map lineToHands (lines contents)
            allHands = concatMap (\(a,b) -> [a, b]) hands
        -- mapM_ print $ filter ((==FourOfAKind) . fst . fst . snd) $ zip hands (map scoreHands hands)
        mapM_ print $ filter ((/=Unknown) . fst . fst) $ zip (map scoreHand allHands) allHands
        -- print (take 1 $ map lineToHands (lines contents))
        )
    -- let hands = lineToHands handsStr
    -- print $ hands
    -- print $ (scoreHand h1, scoreHand h2)
