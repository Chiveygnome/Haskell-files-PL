module Matrix (Matrix, fillWith, fromRule, numRows, numColumns, 
               at, mtranspose, mmap, add, mult) 
where

-- newtype is like "data", but has some efficiency advantages
newtype Matrix a = Mat ((Int,Int),(Int,Int) -> a)

fillWith   :: (Int,Int) -> a -> (Matrix a)
fromRule   :: (Int,Int) -> ((Int,Int) -> a) -> (Matrix a)
numRows    :: (Matrix a) -> Int
numColumns :: (Matrix a) -> Int
at         :: (Matrix a) -> (Int, Int) -> a
mtranspose :: (Matrix a) -> (Matrix a)
mmap       :: (a -> b) -> (Matrix a) -> (Matrix b)
add        :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)
mult       :: Num a => (Matrix a) -> (Matrix a) -> (Matrix a)

-- Without changing what is above, implement the above functions.
fillWith (x,y) z                       = Mat((x,y),(\(a,b) -> if(a>0 && b>0)then z else undefined))
fromRule (x,y) f                       = Mat((x,y),(f))
numRows ( Mat((m,_),(_)))              = m 
numColumns ( Mat((_,n),(_)))           = n
at(Mat((_,_), f)) x                    = f x
mtranspose( Mat((i,j), (f)))           = Mat((j,i), (\ (x,y) ->  f(y,x)))
mmap f (Mat((i,j),(g)))                = (Mat((i,j), f.g))
add (Mat((m,n),(f))) (Mat((i,j),(g)))  = if(m == i && n == j) then Mat((m,n),(\(a,b) -> f (a,b) + g(a,b))) else undefined
mult (Mat((m,n),(f))) (Mat((i,j),(g))) = if(n == i) then Mat((n,i),(\(a,b) -> mult' f g (a,b)  m)) else undefined

mult' f g (x,y) z                     = if(z>0) then f(x,z) * g(z,y) + (mult' f g (x,y) (z-1)) else 0