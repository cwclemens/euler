module Numwriter (writeNum) where
import Data.List (intercalate)

writeNum :: Integer -> String
writeNum = whole . show

units = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
         "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
        "ninety"]
powers = ["", " thousand", " million", " billion", " trillion", " quadrillion",       
          " quintillion", " sextillion", " septillion", " octillion",
          " nonillion", " decillion", " undecillion", " duodecillion", 
          " tredecillion", " quattuordecillion", " quindecillion", " sexdecillion",
          " septendecillion", " octodecillion", " novemdecillion", " vigintillion"]

whole2 :: Int -> String
whole2 = combine2 . (`divMod` 10)
   where combine2 (0,0) = ""
         combine2 (0,a) = units !! (a-1)
         combine2 (1,a) = teens !! a
         combine2 (a,0) = tens !! (a-2)
         combine2 (a,b) = tens !! (a-2) ++ " " ++ units !! (b-1)

whole3 :: Int -> String
whole3 = combine3 . (`divMod` 100)
   where combine3 (0,a) = whole2 a
         combine3 (a,0) = units !! (a-1) ++ " hundred"
         combine3 (a,b) = units !! (a-1) ++ " hundred " ++ whole2 b

whole :: String -> String
whole "0" = "zero"
whole n = combine . reverse . zip [0..] . split $ n
    where split = map (read . reverse) . chunk . reverse
          chunk "" = []
          chunk s | length s > 3 = (\(a,b) -> a : (chunk b)) . splitAt 3 $ s
                  | otherwise = [s]
          combine ns = intercalate " "
                 [whole3 grp ++ (powers !! exp) | (exp,grp) <- ns, grp /= 0]



