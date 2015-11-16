import Prime
import Utils
import Data.List ((\\))

rcat (x,y) = read $ show x ++ show y

pairwise :: (Eq a) => ((a, a) -> Bool) -> [a] -> Bool
pairwise p xs = all p [(x, y) | x <- xs, y <- (xs \\ [x])]

isPrimePairs :: [Integer] -> Bool
isPrimePairs = pairwise (isPrime . rcat)

primeRange x y = dropWhile (<=x) (primesTo y)

ppairs n = filter isPrimePairs [[x,y] | x <- primesTo n, y <- primeRange x n]

ptrips n = filter isPrimePairs [x:pp | pp <- pps, x <- primeRange (maximum pp) n]
  where pps = ppairs n

pquads n = filter isPrimePairs [x:pt | pt <- pts, x <- primeRange (maximum pt) n]
  where pts = ptrips n

pquints n = filter isPrimePairs [x:pq | pq <- pqs, x <- primeRange (maximum pq) n]
  where pqs = pquads n

main = print . head $ pquints 10000
-- ans 26033 from [13,5197,5701,6733,8389]
