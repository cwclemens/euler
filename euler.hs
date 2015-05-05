import Data.List
import Data.Ord (comparing)
import Control.Monad
import Data.Monoid
import Data.Maybe
import qualified Data.Map.Strict as M
import Prime
import System.Environment (getArgs)
import System.IO

ans = f 1 M.empty

f n mp = case M.lookup k mp of
           Just 3 -> (n, k, mp)
           Just _ -> f (n+1) (M.adjust succ k mp)
           Nothing -> f (n+1) (M.insert k 1 mp)
  where k = smCbPerm (n^3) --maxperm (n^3)

--maxperm = reverse . sort . show 

smCbPerm = minimum . filter isCube . map read . filter ((/='0') . head) . permutations . show

isCube x = x == (icbrt x)^3
icbrt = floor . (+0.000001) . (**(1/3)) . fromIntegral
