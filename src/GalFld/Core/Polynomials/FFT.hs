{-# LANGUAGE BangPatterns #-}
module GalFld.Core.Polynomials.FFT
  where

import Data.List
import qualified Control.Arrow as A
import GHC.Integer.Logarithms
import GalFld.Core.Polynomials
import Debug.Trace



-------------------------------------------------------------------------------
-- FFT


-- |Berechnet die FFT eines Polynoms f
--  Benötigt eine primitive n-te Einheitswurzel,
--  wobei n eine 2er Potenz ist (Dies wird NICHT überprüft!)
--  Diese wird dargestellt als Funktion w: Int -> a -> a,
--  wobei f(i,x) = w^i*x für die n-te EWL w auswertet.
--  
--  vgl Computer Algebra Algorithmus 4.11
fftP :: (Show a, Num a, Eq a) => (Int -> a -> a) -> Int -> Polynom a -> [a]
fftP w n f = fft w (+) (-) 0 1 n (p2List f)


-- | Int -> a -> a  Implementierung der primitiven Einheitswurzel
--   a -> a -> a    Addition auf a
--   a -> a -> a    Subtraktion auf a
--   -> a           Die Null
--   -> Int         Aktuelle 2er Potenz (Starte mit 1)
--   -> Int         FFT bis n
--   -> [a]         Eingangsliste
--   -> [a]         Ausgabeliste
fft :: (Show a) => (Int -> a -> a) -> (a->a->a) -> (a->a->a) -> a
                                                -> Int -> Int -> [a] -> [a]
fft _ _ _ _ _ 1 fs = --trace ("fftEnd: fs="++show fs) $ 
             fs
fft w addF subF zero i n fs = --trace ("fft: n="++show n++" m="++show m++" fs="++show fs++" fss="++show fss
               -- ++"\n\t=>ls="++show ls++" rs="++show rs) $ 
             intersperseL ls' rs'
  where !i'  = 2*i
        !ls' = fft w addF subF zero i' m ls
        !ls  = take m $ zipWith' (addF) zero fs fss
        !rs' = fft w addF subF zero i' m rs
        !rs  = take m $ zipWith (w) [i | i<-[0..]] $ zipWith' (subF) zero fs fss
        !fss = drop m fs
        !m   = n `quot` 2


-------------------------------------------------------------------------------
-- Schönhagen-Strassen Multiplikation


{-# INLINE ssP #-}
-- | Schönhagen-Strassen für Polynome
ssP :: (Show a, Fractional a, Num a, Eq a) => Polynom a -> Polynom a -> Polynom a
ssP f g  = pList $ ss l fs gs
  where fs = p2List f
        gs = p2List g
        -- || deg f*g < 2^l ||
        l  = 1 + log2 (uDegP f + uDegP g)


-- |Der eigentliche Schönhagen-Strassen Algorithmus
--  Funktioniert nur, falls 2 eine Einheit ist!
ss :: (Show a, Num a, Fractional a, Eq a) => Int -> [a] -> [a] -> [a]
-- ss funktioniert nur für l>2
ss 1 f g = multLists f g
ss 2 f g = multLists f g
ss l f g 
  | (all (==0) f) || (all (==0) g) = [] 
  | otherwise = trace ("ss with l="++show l++" l'="++show l'++" m="++show m++" m'="++show m'
      ++"\nf="++show f++"\ng="++show g
      ++"\n\t fs="++show fs++" gs="++show gs
      ++"\n\t fs'="++show fs'++" gs'="++show gs'
      ++"\n\t fftFs="++show (map (pList) fftFs)++"\n\tffTGs="++show (map pList fftGs)
      ++"\n\t -> ffTHs"++show (map pList fftHs)
      ++"\n\t xi*(2*m'-2)="++show (xi*(2*m'-2))++" hs''="++show (map pList hs'')
      ++"\n\t hs'="++show hs'++" \n\txi*(2*m'-1)="++show (xi*(2*m'-1)) 
      ++" hs'''="++show (map (p2Tup . pList) (zipWith (multx (xi*(2*m'-1))) [0..] hs' ))++" hs="++show (map pList hs)) $
      foldr1 (zipWith' (+) 0) $  reduceModxn (2^l) $ zipWith (multx (m)) [0..] hs
  where -- << n = 2^l = m * m' >>
        !l' = l `quot` 2
        !m  = 2^l'
        !m' = 2^(l-l')
        !fs = ssBuildBlocks (m*(m'-1)) m $ zip [0..] f
        !gs = ssBuildBlocks (m*(m'-1)) m $ zip [0..] g 
        -- auf FFT vorbereiten
        !fs' = zipWith (multx (xi)) [0..] fs
        !gs' = zipWith (multx (xi)) [0..] gs 
        {-fs' = zipWith (++|) [take i $ cycle [0] | i<-[0..]] fs-}
        {-gs' = zipWith (++|) [take i $ cycle [0] | i<-[0..]] gs-}
        -- FFT durchführen
        !xi    = if odd l then 1 else 2
        !fftFs = reduceModxn (2*m) $ fft (multx (xi*2)) (zipWith' (+) 0) (zipWith' (-) 0) [0] 1 m' fs'
        !fftGs = reduceModxn (2*m) $ fft (multx (xi*2)) (zipWith' (+) 0) (zipWith' (-) 0) [0] 1 m' gs'
        -- Multiplikation der Ergebnisse und rekursiver Aufruf von ss
        !fftHs = reduceModxn (2*m) $ zipWith (ss (l'+1)) fftFs fftGs
        {-fftHs = [[4,4,3,4],[3,3,3,4],[3,3,2,4],[4,4,4,3]]-}
        -- Inverse-FFt
        !hs''  = reduceModxn (2*m) $ fft (multx (xi*(2*m'-2))) (zipWith' (+) 0) 
                                                (zipWith' (-) 0) [0] 1 m' fftHs
        -- * 1/m'
        !hs'   = map (map (\x -> x / (fromIntegral m'))) hs''
        -- Rückwandlung zu H(x,y)
        !hs    = reduceModxn (2*m) $ zipWith (multx (xi*(2*m'-1))) [0..] hs'


{-[># INLINE reduceModxn #<]-}
-- | Reduziert die innere Liste modulo x^n+1
reduceModxn :: (Show a, Num a) => Int -> [[a]] -> [[a]]
reduceModxn _ [] = --trace ("reduceModxn empty") $ 
                    []
reduceModxn n x@(xs:xss) 
    | l > n     = --trace ("reduceModxn n="++show n++" l="++show l++" l>n") $ 
                  reduceModxn n (ys:xss) 
    | otherwise = --trace ("reduceModxn n="++show n++" l="++show l++" l<=n x="++show x++" xs="++show xs++" xss="++show xss)$ 
                  xs : reduceModxn n xss
  where l = length xs
        ys = zipWith' (-) 0 (take n xs) (drop n xs)


{-[># INLINE ssBuildBlocks #<]-}
ssBuildBlocks :: (Show a, Eq a, Num a) => Int -> Int -> [(Int,a)] -> [[a]]
ssBuildBlocks _ _ [] = []
ssBuildBlocks 0 _ fs = [checkEmpty $ map snd fs]
ssBuildBlocks n m fs = --trace ("fs="++show fs++" n="++show n++" m="++show m
                       -- ++"\n\t=>ms'="++show ms'++" ns="++show ns) $
                      (ssBuildBlocks (n - m) m ns) ++ [checkEmpty ms]
  where ms' = filter (\(i,x) -> i >= n) fs
        ns  = fs \\ ms'
        ms  = map (snd) ms'


{-[># INLINE multx #<]-}
-- | Multipliziert mit x^(i*j)
multx :: (Num a) => Int -> Int -> [a] -> [a]
multx _ _ []    = []
multx j i xs 
    | k < 0     = drop k xs
    | otherwise = (take k $ cycle [0]) ++ xs
  where k = j*i


-------------------------------------------------------------------------------
-- Helper

{-[># intersperseL #<]-}
-- |Intersperse mit 2 Listen
intersperseL :: [a] -> [a] -> [a]
intersperseL ys   []      = ys
intersperseL []   xs      = xs
intersperseL (y:ys) (x:xs)  = y : x : intersperseL ys xs



{-# INLINE zipWith' #-}
-- like @zipWith@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipWith' :: (t->t->t) -> t -> [t] -> [t] -> [t]
zipWith' _ _ xs [] = xs
zipWith' f t [] ys = map (f t) ys
zipWith' f t (x:xs) (y:ys) = (f x y) : zipWith' f t xs ys



{-[># INLINE log2 #<]-}
-- |ineffiziente Log 2 Berechnung
log2 :: Int -> Int
log2 0 = 0
log2 1 = 0
log2 n = log2' 1 n
  where log2' i 1 = max 0 (i-1)
        log2' i 2 = i
        log2' i n = 1 + (log2' i $! n `quot` 2)

{-[># INLINE checkEmpty #<]-}
-- | Prüft, ob eine Liste nur 0en enthält.
checkEmpty :: (Num a, Eq a) => [a] -> [a]
checkEmpty xs | all (==0) xs = []
              | otherwise   = xs



-- |Aus Math.Polynomial
{-[># INLINE multLists #<]-}
multLists :: (Eq a, Num a) => [a] -> [a] -> [a]
multLists  _  []     = []
multLists  []  _     = []
multLists  xs (y:ys) = foldr mul [] xs
    where
        mul x bs
            | x == 0      = 0 : bs
            | otherwise  = (x * y) : zipSum (map (*x) ys) bs



{-[># INLINE zipSum #<]-}
-- like @zipWith (+)@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipSum :: Num t => [t] -> [t] -> [t]
zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys

