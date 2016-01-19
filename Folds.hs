module Folds where


count :: Eq a => a -> [a] -> Integer
-- Put your implementation here
count _ []       = 0
count num (n:ns) = foldl(\ y -> if(n==num) then (y+1) else y) 0 ns
elem2 :: Eq a => a -> [a] -> Bool
-- Put your implementation here
elem2 _[]        = False
elem2 num xs = foldr(\f y-> if(y == num) then True else f) False xs