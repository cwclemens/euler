import Data.Ratio
--import Data.Set

b = 3 % 7

--allFrac d = toAscList $ fromList [a%b | b <- [1..d], a <- [0..b]]

score q = abs (b-q)
neighbors n m = map (+n) [-m..m]

getCloser (x,y) = (\q->(numerator q, denominator q)) . maximum $ filter (<b) [(x'% y') | x' <- neighbors x 100, y' <- neighbors y 100, y' <= 1000000] 

--428570/999997
