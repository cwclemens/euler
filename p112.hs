
main = print . head . dropWhile ((\(a,b)->a/=99*b).snd) 
           $ zip [1..] (counts [1..])

counts = counts' (0,0)
counts' (a,b) (x:xs) | isBouncy x = (a+1,b) : counts' (a+1,b) xs
                     | otherwise  = (a,b+1) : counts' (a,b+1) xs

isBouncy n = not (increasing || decreasing)
  where s = show n
        t = zip s (tail s)
        increasing = and $ map (uncurry (<=)) t
        decreasing = and $ map (uncurry (>=)) t
