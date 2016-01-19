module Zip3unzip3 where
import Prelude hiding (zip3, unzip3)

zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
-- Put your implementation here
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []
unzip3 :: [(a,b,c)] -> ([a], [b], [c])
-- Put your implementation here
unzip3 [] = ([],[],[])
unzip3 ((a,b,c):xs) = (a:as, b:bs, c:cs)
                where (  as,   bs,   cs) = unzip3 xs