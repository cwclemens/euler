import qualified Data.Set as S

factorial 0 = 1
factorial n = n * factorial (n-1)

dfacts = map factorial [0..9]

digitFactorial :: Integer -> Integer
digitFactorial = sum.map ((dfacts!!).read.(\x->[x])).show

dfChain n = n : dfChain (digitFactorial n)

dfPeriod n = dfPeriod' 0 S.empty (dfChain n)
  where dfPeriod' k mp (b:bs) | b `S.member` mp = k
                              | otherwise = dfPeriod' (k+1) (S.insert b mp) bs

main = print.length.filter ((==60).dfPeriod) $ [1..999999]
-- 402


