module Prime (primes,primesTo,factors,facMult,countDivisors,divisors,isPrime) where
import Data.List

-- infinite list of primes using sieve of eratosthenes
primes = 2 : primes'
  where primes' = sieve [3,5..] 9 primes'
        sieve (x:xs) q ps@ ~(p:t)
          | x < q = x : sieve xs q ps
          | otherwise = sieve [x | x <- xs, rem x p /= 0] (head t^2) t

primesTo n = takeWhile (<=n) primes

factors n | null xs = [n]
          | otherwise = x : factors (div n x)
        where xs@ ~(x:_) = [y | y <- (primesTo imax), rem n y == 0]
              imax = floor . sqrt $ fromIntegral n

facMult = foldr f [] . factors
        where f p [] = [(p,1)]
              f p xs@((x,n):t) | p == x = (x,n+1):t
                               | otherwise = (p,1):xs

countDivisors = product . map ((+1) . snd) . facMult

divisors n = sort . map product $ sequence [powers p mult | (p,mult) <- facMult n]
        where powers p mult = map (p^) [0..mult]

-- fermat primality test, witnesses must be small primes
fermat [] _ = True
fermat as'@(a:as) n | n `elem` as' = True
                    | otherwise = ((powmod a (n-1) n) == 1) && (fermat as n)

powmod = powmod' 1
  where powmod' r _ 0 _ = r
        powmod' r b e m = powmod' (if odd e then r*b `rem` m else r) (b*b `rem` m) (e `quot` 2) m

isPrime 0 = False
isPrime 1 = False
isPrime n = (fermat $ take 20 primes) n


