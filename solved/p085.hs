import Data.List (minimumBy)
import Data.Ord (comparing)

-- (n+1)C2 * (m+1)C2
countRect n m = n*(n+1)*m*(m+1) `quot` 4

score (n,m) = abs (countRect n m - 2000000)

-- (36,77,2772)
main = print . (\(a,b) -> (a, b, a*b)) $
     minimumBy (comparing score) [(a,b) | a <- [1..100], b <- [1..100]]
