
odd' = (`elem` ['1','3','5','7','9'])
nd10 = (/='0') . last

reversible n = (nd10 s) && (all odd' rsum)
  where s = show n
        rev = read . reverse $ s
        rsum = show (n + rev)

count p = length . filter p

main = print . count reversible $ [1..1000000000]
