module Unique where
unique :: (Eq a)  => [a] -> [a]
unique y              = unique' y []
   where
      unique' []_     = []
      unique' (x:xs) ys
        | x `elem` ys = unique' xs ys
        |otherwise    = x : unique' xs (x:ys)
