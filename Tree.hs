module Tree where

data Tree a = Nil | Node a (Tree a) (Tree a) 
                deriving (Eq, Show)

depth :: Tree a -> Integer
depth Nil            = 0
depth (Node n t1 t2) = 1 + max (depth t1) (depth t2)

-- collapses a tree into a list by visiting 
-- the elements of the tree 'inorder'

collapse :: Tree a -> [a]
collapse Nil            = []
collapse (Node x t1 t2) = collapse t1 ++ [x] ++ collapse t2

-- stratifies a tree into a list by visiting
-- all elements at depth 1, then all elements 2, etc.

stratify :: Tree a -> [a]
-- This solution was inspired with help from stackoverflow
-- Put your implementation here
stratify Nil            = []
stratify x              = stratify' [x] 
     where stratify' []                  = []
           stratify'( Nil           :xs) = stratify' xs
           stratify'((Node n n1 n2) :xs) = n: stratify'(xs++[n1,n2])