import Data.List
import Data.Maybe
import System.IO

main = do
       text <- readFile "p054_poker.txt"
       let games = map (splitAt 5 . words) . lines $ text
           wins = filter p1won games
       putStrLn $ "Player 1 won " ++ (show $ length wins) ++ 
                  " of " ++ (show $ length games) ++ " hands."

-- Data -------------------------------------------------------------
type Deal = [String] -- ["RS"] Rank Suit

data Hand = HC [Rank] | P1 Rank | P2 Rank Rank | TK Rank 
            | ST Rank | FL | FH Rank Rank | FK Rank | SF Rank | RF
  deriving (Ord, Eq, Show)
  
data Rank = One | Two | Three | Four | Five | Six | Seven 
            | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Ord, Enum, Eq, Show)

getRanks :: [Char] -> [Rank]
getRanks = mapMaybe (`lookup` ranktable)
  where ranktable = zip "AKQJT98765432" stOrder
stOrder = [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six,
            Five, Four, Three, Two, Ace, Five, Four, Three, Two]

-- Funcs ------------------------------------------------------------
p1won :: (Deal, Deal) -> Bool
p1won (p1,p2) = score p1 > score p2

score :: Deal -> [Hand]
score deal = rsort $ straightsAndFlushes ++ pairs ++ high
  where ranks = rsort . getRanks $ map head deal
        high = [HC (nub ranks)]
        pairs = getPairs ranks
        straight = if ranks `isInfixOf` stOrder then [ST (head ranks)] else []
        isFlush = (\(x:xs) -> all (==x) xs) $ map last deal
        straightsAndFlushes | isFlush = case straight of
                                       [ST r] -> if r==Ace && last ranks > Five 
                                                 then [RF] else [SF r]
                                       [] -> [FL]
                            | otherwise = straight

getPairs :: [Rank] -> [Hand] --Input must be sorted descending
getPairs = getPairs' . rsort . map (\x -> (length x, head x)) . group
  where getPairs' [] = []
        getPairs' ((n,r):t) | n==4 = [FK r]
                            | n==3 = case getPairs' t of
                                [P1 x] -> [FH r x]
                                [] -> [TK r]
                            | n==2 = case getPairs' t of 
                                [P1 x] -> [P2 r x]
                                [] -> [P1 r]
                            | otherwise = []

rsort :: (Ord a) => [a] -> [a]
rsort = reverse . sort

