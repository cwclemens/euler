import Data.Maybe (fromJust)
import Control.Monad (liftM2)

value :: Char -> Integer
value c = case lookup c tbl of
           Just n -> n
           Nothing -> 0
  where tbl = zip ['I','V','X','L','C','D','M'] [1,5,10,50,100,500,1000]

parseNumeral :: String -> Integer
parseNumeral s = parse (reverse.map value $ s)
  where parse [] = 0
        parse [n] = n
        parse (x:y:ys) | y<x = x - y + parse ys
                       | otherwise = x + parse (y:ys)

toNumeral :: Integer -> String
toNumeral n = toNumeral' n "" 
  where toNumeral' 0 s = s
        toNumeral' k s = let (l, c) = g k in toNumeral' (k-l) (s++c)
        g k = let t = v k in (t, u t)
        v k = maximum $ filter (<=k) vals
        u k = fromJust $ lookup k (zip vals elts)
        vals = [1,4,5,9,10,40,50,90,100,400,500,900,1000]
        elts = ["I","IV","V","IX","X","XL","L","XC","C","CD","D","CM","M"]

savings = liftM2 (-) length (length.toNumeral.parseNumeral)

main = do
       numerals <- fmap lines (readFile "p089_roman.txt")
       print $ sum.map savings $ numerals
-- 743
