module AddList where
add_list_comprehension        :: Integer -> [Integer] -> [Integer]
add_list_comprehension x (xs) = [xs+x|xs<-xs]
add_list_recursion            :: Integer -> [Integer] -> [Integer]
add_list_recursion i []       = []
add_list_recursion i (x:xs)   = x + i : add_list_recursion i xs
add_list_map                  :: Integer -> [Integer] -> [Integer]
f x y = x + y
add_list_map x xs             = map (f x) xs