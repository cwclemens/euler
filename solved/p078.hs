import Control.Monad.State
import qualified Data.Map as M
import Data.List (foldl')

-- naive implementation

q :: Int -> Int -> Int
q 0 0 = 1
q 1 1 = 1
q n m = sum [q n' (min n' g) | g <- [1..m], let n' = n-g] 

p n = q n n

-- memoized implementation -- still too slow

type QMap = M.Map (Int,Int) Int

mq' :: Int -> Int -> State QMap Int
mq' n m = do 
     qm <- get
     case M.lookup (n,m) qm of
       Just t -> return t
       Nothing -> do
         xs <- sequence [mq' n' (min n' g) | g <- [1..m], let n' = n-g]
         let x = sum xs `mod` 1000000
         modify $ M.insert (n,m) x
         return x
               
qm0 = M.fromList [((0,0),1),((1,1),1)]
mp' n = mq' n n
mp n = fst . runState (mp' n) $ qm0
mps = fst $ runState (sequence $ map mp' [1..]) qm0

-- using Pentagonal number theorem

pent k = k*(3*k-1) `div` 2
pairs (x:xs) (y:ys) = x : y : pairs xs ys
pents n = takeWhile (\i->n>=i) . map pent $ pairs [1..] [-1,-2..]
signs = cycle [1,1,-1,-1]

type PMap = M.Map Int Int

pm0 = M.fromList [(0,1),(1,1)]
pp' :: Int -> State PMap Int
pp' n = do
    pm <- get
    case M.lookup n pm of
      Just t -> return t
      Nothing -> do
        xs <- sequence [pp' (n-i) | i <- pents n]
        let x = (`mod` 1000000).sum $ zipWith (*) signs xs
        modify $ M.insert n x
        return x
pps = fst $ runState (sequence $ map pp' [1..]) pm0

-- main

main = mapM_ print .
       takeWhile ((/=0).snd)
       $ zip [1..] pps
 --55374        

