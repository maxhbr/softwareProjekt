{-# GHC_OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE BangPatterns #-}
import qualified Control.Arrow as A


-- | Multiplikation von absteigend sortierten [(Int,a)] Listen
multPM :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
multPM  ms  []     = []
multPM  []  ns     = []
multPM  ((i,m):ms) ns  = addPM  a b
  where !a = multPM' i m ns
        !b = multPM ms ns

multPM' i m []         = []
multPM' i m ((j,n):ns) = (k,c) : multPM' i m ns
  where !c = n*m
        !k = i+j  


addPM [] gs          = gs
addPM fs []          = fs
addPM ff@((i,f):fs) gg@((j,g):gs)
  | i==j && c/=0  = (i,c) : addPM fs gs
  | i==j && c==0  = addPM fs gs
  | i<j         = (j,g) : addPM ff gs
  | i>j         = (i,f) : addPM fs gg
   where !c = f+g

{-# INLINE multP'' #-}
multP'' :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
multP''  _  []     = []
multP''  []  _     = []
multP''  xs ((j,y):ys) = foldr mul [] xs
    where
        mul :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
        mul (i,x) bs
            | x == 0 =  bs
            | otherwise  = ((x * y),i+j) : addPM (map (A.first (*x)) ys) bs

{-# INLINE multP #-}
multP :: (Eq a, Num a) => [a] -> [a] -> [a]
multP  _  []     = []
multP  []  _     = []
multP  xs (y:ys) = foldr mul [] xs
    where
        mul !x !bs
            | x == 0      = 0 : bs
            | otherwise  = (x * y) : zipSum (map (*x) ys) bs


{-# INLINE zipSum #-}
-- like @zipWith (+)@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipSum :: Num t => [t] -> [t] -> [t]
zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys



f = [(11::Int,1::Int),(8,2),(6,1),(5,1),(3,2),(2,2),(0,1)]


heavyBench :: [(Int,Int)] -> Int -> [(Int,Int)]
heavyBench f 0 = f
heavyBench f n = multPM f $! heavyBench f (n-1)

main = print $ last $! heavyBench f 500
