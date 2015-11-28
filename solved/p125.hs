import Control.Applicative 
import Data.List

squares = scanl1 (+) [1,3..]
sqrAcc = scanl1 (+) squares
sqrAcn n = takeWhile (<n) sqrAcc
palindromic n = show n == reverse (show n)

psums = nub . sort .
        filter ((&&) <$> (<100000000) <*> palindromic)
        $ [n-m | n <- sqrAcn 100000000000, m <- init (0 : sqrAcn n)]

main = print $ sum psums
-- 2906969179
