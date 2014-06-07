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
  , modMonom, factor
)	where

import Projekt.Core
import Debug.Trace
import Data.List


-- |Filtert mittels Rabin aus einer Liste irreduziblen Polynome heraus
findIrredsRabin :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [Polynom a]
findIrredsRabin ff@(f:fs) = findIrreds' ff
  where findIrreds' []     = []
        findIrreds' (f:fs) 
          | fR        = f : findIrreds' fs
          | otherwise = findIrreds' fs
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
rabin :: (Show a, FiniteField a, Num a, Fractional a, Eq a) => Polynom a -> Bool
rabin f = --trace ("rabin with f="++show f++" d="++show d++" q="++show q++" ns="++show ns) $
    rabin' f ns
  where ns = map (\p -> d `quot` p) $ nub $ factor d
        d  = uDegP f
        q  = elemCount $ getReprP f
        pX = pTupUnsave [(1,1)]
        -- eigentlicher Rabin für den letzen Test mit x^(q^n) - x
        rabin' f [] = --trace ("rabin' d="++show d++" h="++show h++" h mod f="++show (modByP h f)++" => ggT="++show g) $ 
                      isNullP g
          where g  = (h'-pX) `modByP` f
                h' = modMonom (q^d) f
        -- eigentlicher Rabin für x^(q^n_j) - x mit n_j = n / p_j
        rabin' f (n:ns) | g /= pKonst 1  = --trace ("rabin' n="++show n++" g="++show g) 
                                      False
                        | otherwise = --trace ("rabin' d="++show d++" h="++show h++" h mod f="++show (modByP h f)++" => ggT="++show g) $
                                      rabin' f ns
          where g  = (ggTP f $ h'-pX)
                h' = modMonom (q^n) f

-------------------------------------------------------------------------------
-- Helper


-- |Primfaktorzerlegung 
--  aus http://www.haskell.org/haskellwiki/99_questions/Solutions/35
factor :: Integral a => a -> [a]
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime



-- | Schnelles Modulo für Monome, d.h. berechnet
--   x^n mod f
modMonom :: (Show a, Num a, Eq a, Fractional a) => 
                                                Int -> Polynom a -> Polynom a
modMonom n f | n < df    = pTupUnsave [(n,1)]
             | even n    = g `modByP` f
             | otherwise = (multMonomP 1 g) `modByP` f
  where df = uDegP f
        m  = n `quot` 2
        g  = (modMonom m f) * (modMonom m f)
