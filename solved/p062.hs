import Data.List (sort)
import qualified Data.Map.Strict as M

digitMap :: M.Map String [Integer]
digitMap = M.fromListWith (++) (map pr xs)
  where xs = [1..20000]
        dig = sort.show.(^3)
        pr v = (dig v, [v])

main = print.disp.minimum.head $ quintuples
  where quintuples = M.elems $ M.filter ((==5).length) digitMap
        disp x = show x ++ "^3 => " ++ show (x^3)
-- 5027^3 => 127035954683
