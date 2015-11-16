module Utils where
import Data.List (foldl', sort, (\\))

count p (x:xs) | p x = 1 + count p xs
               | otherwise = count p xs
count p _ = 0

argmax :: (Ord b) => (a -> b) -> [a] -> (a,b)
argmax func (t:ts) = foldl' f (t, func t) ts
  where f (x,fx) y | fy <- func y, fy > fx = (y,fy)
                   | otherwise = (x,fx)

factorial n = product [1..n]

choose n m = factorial n `quot` (factorial m * factorial (n-m))

compose [] = id
compose fns = foldr1 (.) fns

removeAll p [] = []
removeAll p (x:xs) | p x = removeAll p xs
                   | otherwise = x : removeAll p xs


split p s = x1:x2
     where (x1,x2) = foldr func ([],[]) s
           func t (cs,ds) | p t = ([],cs:ds)
                          | otherwise = (t:cs,ds)

sumDiff [] _ = 0
sumDiff xs [] = sum xs
sumDiff (x:xs) yss@(y:ys) | x == y = sumDiff xs ys
                      | otherwise = x + sumDiff xs yss

dedup [] = []
dedup [x] = [x]
dedup (x:y:ys) | x == y = dedup (y:ys)
               | otherwise = x : dedup (y:ys) 

permutations [x] = [[x]]
permutations xs = [x:p | x <- xs, p <- permutations (xs\\[x])]

pandigital s = sort s == digits
  where digits = ['0'..'9']

allIn [] [] = True
allIn [] _ = False
allIn _ [] = True
allIn (y:ys) (x:xs) | x == y = allIn ys xs
                    | otherwise = allIn ys (x:xs)

decToBin :: Int -> String
decToBin x = concatMap show (dtb x [])
  where dtb 0 bin = bin
        dtb quo bin = dtb q (r:bin)
          where (q,r) = quotRem quo 2

isqrt = floor . sqrt . fromIntegral
isSquare n = n == (isqrt n)^2

-- dates

incrDate (dy,da,mo,yr) = (dy',da',mo',yr')
  where dy' = (dy+1) `rem` 7
        da' = (da+1) `rem` daymod
        mo' = if da'==0 then (mo+1) `rem` 12 else mo
        yr' = if mo'==0 && da'==0 then yr+1 else yr
        daymod = modat yr !! mo

modat yr = [31,(if leap then 29 else 28),31,30,31,30,31,31,30,31,30,31]
  where leap = not $ (rem yr 4 /= 0) || ((rem yr 100 == 0) && (rem yr 400 /= 0))
