import Prime (distinctFactors)
import Data.Ord (comparing)
import Data.List (sortBy)

rad = product . distinctFactors

radPair n = (n, rad n)

main = print $ (!! 9999) . 
         sortBy (comparing snd) . map radPair $ [1..100000]
-- 21417
