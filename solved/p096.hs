import Data.Array.IArray
import qualified Data.Set as S
import Data.List (splitAt, (\\), foldl1')

-- 24702
main = do grids <- input
          let solns = map (head.solve) grids
          print . sum $ map tag solns
  where tag g = read.concat.map show.take 3 $ row g 0

input :: IO [Grid]
input = readFile "p096_sudoku.txt" >>=
          return.map readgrid.chunk.lines
  where readgrid = grid.map readline
        readline = map (read . (\x->[x]))
        chunk [] = []
        chunk ls = let (x,xs) = splitAt 10 ls
                   in (tail x) : chunk xs

coords = [(a,b) | b <- [0..8], a <- [0..8]]

type Grid = Array (Int, Int) Int
grid :: [[Int]] -> Grid
grid = array ((0,0), (8,8)) . zip coords . concat
        
row g n = [g ! (i,n) | i <- [0..8]]
col g n = [g ! (n,i) | i <- [0..8]]
blc g n = [g ! (a,b) | b <- [y,y+1,y+2], a <- [x,x+1,x+2]]
  where (y',x') = n `divMod` 3
        (y,x) = (3*y', 3*x')

possible :: Grid -> (Int, Int) -> [Int]
possible g (i,j) = [1..9] \\ S.elems s
  where set = S.fromList . filter (/=0)
        s = S.unions $ map set [row g j, col g i, blc g k]
        k = let (i',j') = (div i 3, div j 3) in i'+3*j'

deduce :: Grid -> [Grid]
deduce g | null zs = [g]
         | l == 0 = []
         | l == 1 = deduce (g // [(ij,v)])
         | otherwise = [g]
  where zs = zeros g
        (ij,l) = argmin (length.possible g) zs
        (v:_) = possible g ij

zeros g = filter ((==0).(g !)) coords
complete g = all (/=0) (elems g)
pivot g = fst $ argmin (length.possible g) (zeros g)

solve :: Grid -> [Grid]
solve g | complete g = [g]
        | otherwise = 
            untilM complete g0 (\g1 -> do
              let ij = pivot g1
              v <- possible g1 ij
              deduce (g1 // [(ij,v)])
            )
  where [g0] = deduce g

untilM p x f | p x = return x
             | otherwise = do
                 x' <- f x
                 untilM p x' f

argmin :: (Ord b) => (a -> b) -> [a] -> (a,b)
argmin f xs = foldl1' (minBy snd) ys
  where ys = zip xs (map f xs)
        minBy p x y = if p x < p y then x else y
