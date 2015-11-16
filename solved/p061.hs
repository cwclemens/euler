import Utils (isqrt)
import qualified Data.Map.Strict as M
import Data.List (sort)
import Data.Maybe (fromJust)

isNAngular n x = rt*rt == d && (rt-b) `rem` (2*a) == 0
  where a = n - 2
        b = 4 - n
        c = -2*x
        d = b*b - 4*a*c
        rt = isqrt d

typeMap :: M.Map Integer [Integer]
typeMap = M.fromAscList $ map pair polys
  where ns = [3..8]
        comp x = map ($x) (map isNAngular ns)
        typ x = map snd . filter fst $ zip (comp x) ns
        poly = not . null . typ
        polys = filter poly [1000..9999]
        pair x = (x, typ x)

fetch :: Integer -> Integer -> (Integer, Maybe [Integer])
fetch x y = (k, M.lookup k typeMap)
  where k = read $ show x ++ show y

soln = [[ab,bc,cd,de,ef,fa] |
        a <- [10..99],
        b <- [10..99],
        let (ab,tab) = fetch a b,
        tab /= Nothing,
        c <- [10..99],
        let (bc,tbc) = fetch b c,
        tbc /= Nothing,
        d <- [10..99],
        let (cd,tcd) = fetch c d,
        tcd /= Nothing,
        e <- [10..99],
        let (de,tde) = fetch d e,
        tde /= Nothing,
        f <- [10..99],
        let (ef,tef) = fetch e f,
        tef /= Nothing,
        let (fa,tfa) = fetch f a,
        tfa /= Nothing,
        let s = map fromJust [tab,tbc,tcd,tde,tef,tfa],
        [3..8] `elem` map sort (sequence s)]

main = putStrLn $ case soln of
                 [] -> "No Solution Found"
                 (x:_) -> show x ++ "=>" ++ show (sum x)
--[1281,8128,2882,8256,5625,2512]=>28684
