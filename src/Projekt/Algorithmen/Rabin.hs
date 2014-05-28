--------------------------------------------------------------------------------
-- |
-- Module      : Project.Algorithmen.Rabin
-- Note        : Implementiert Rabin's Irreduzibilitätstest
--
--
--------------------------------------------------------------------------------

module Projekt.Algorithmen.Rabin (
  rabin
  , findIrredsRabin
)	where

import Projekt.Core
import Debug.Trace


-- |Filtert mittels Rabin aus einer Liste irreduziblen Polynome heraus
findIrredsRabin :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [Polynom a]
findIrredsRabin ff@(f:fs) = findIrreds' ff
  where findIrreds' []     = []
        findIrreds' (f:fs) 
          | (not (hasNs f es)|| uDegP f < 2) && fR = f : findIrreds' fs
          | otherwise                              = findIrreds' fs
          where fR = rabin f
                es = elems $ getReprP f

-- |Rabin's Irreduzibilitätstest
--  Ausgabe: True, falls f irreduzibel, False, falls f reduzibel
--
--
--  Algorithm Rabin Irreducibility Test
--  Input: A monic polynomial f in Fq[x] of degree n, 
--         p1, ..., pk all distinct prime divisors of n.
--  Output: Either "f is irreducible" or "f is reducible".
--  Begin
--      for j = 1 to k do 
--         n_j := n / p_j;
--      for i = 1 to k do 
--         h := x^(q^n_i) - x  mod f;
--         g := gcd(f, h);
--         if g ≠ 1, then return 'f is reducible' and STOP;
--      end for;
--      g := x^(q^n) - x  mod f;
--      if g = 0, then return "f is irreducible", 
--          else return "f is reducible"
--  end.
--  aus http://en.wikipedia.org/wiki/Factorization_of_polynomials_over_
--      finite_fields#Irreducible_polynomials
rabin :: (Show a, FiniteField a, Num a, Fractional a) => Polynom a -> Bool
rabin f = --trace ("rabin with f="++show f++" d="++show d++" q="++show q++" ns="++show ns) $
    rabin' f ns 0 pX
  where ns = map (\p -> d `quot` p) $ factor d
        d  = uDegP f
        q  = elemCount $ getReprP f
        pX = pTupUnsave [(1,1)]
        rabin' f [] m h                = --trace ("rabin' d="++show d++" h="++show h++" h mod f="++show (modByP h f)++" => ggT="++show g) $ 
                                          isNullP g
          where g = h' `modByP` f
                h' = h^(q^(d-m)) - pX
        rabin' f (n:ns) m h | g /= pKonst 1  = --trace ("rabin' n="++show n++" g="++show g) 
                                          False
                            | otherwise = --trace ("rabin' d="++show d++" h="++show h++" h mod f="++show (modByP h f)++" => ggT="++show g) $
                                          rabin' f ns n h'
          where g  = (ggTP f $ (h' `modByP` f)- pX)
                h' = h^(q^(n-m))


{-rabin :: (Show a, FiniteField a, Num a, Fractional a) => Polynom a -> Bool-}
{-rabin f = or $ (map (\p -> rab p) $ factor d) ++ -}
        {-[(ggTP f $ fromMonomialsP [(q^d,1),(1,-1)]) `modByP` f == 0]-}
  {-where d  = uDegP f -}
        {-q  = elemCount $ getReprP f-}
        {-rab p = (ggTP f $ fromMonomialsP [(q^(d `quot` p),1),(1,-1)]) `modByP` f /= 1-}


-- |Primfaktorzerlegung 
--  aus http://www.haskell.org/haskellwiki/99_questions/Solutions/35
factor :: Integral a => a -> [a]
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime

