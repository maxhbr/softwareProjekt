--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Algorithmen.Rabin
-- Note        : Implementiert Rabin's Irreduzibilitätstest
--
--
--------------------------------------------------------------------------------

module GalFld.Algorithmen.Rabin (
  rabin
  , getMonicIrredRabin
  , findIrredRabin, findIrredsRabin
  , modMonom, factor
  ) where

import GalFld.Core
import Debug.Trace
import Data.List

-- |Gibt ein irreduzibles monisches Polynom von Grad n über dem Körper von x
getMonicIrredRabin :: (Show a, Num a, Fractional a, FiniteField a) => 
                                                        a -> Int -> Polynom a
getMonicIrredRabin x n = findIrredRabin $ getAllMonicPs (elems x) [n]


-- |Wählt aus einer Liste von Polynomen das erste Irreduzibele Polynom heraus
findIrredRabin :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> Polynom a
findIrredRabin = head . findIrredsRabin

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
rabin f = rabin' f ns
  where ns = map (\p -> d `quot` p) $ nub $ factor d
        d  = uDegP f
        q  = elemCount $ getReprP f
        pX = pTupUnsave [(1,1)]
        -- eigentlicher Rabin für den letzen Test mit x^(q^n) - x
        rabin' f [] = isNullP g
          where g  = (h'-pX) `modByP` f
                h' = modMonom q d f
        -- eigentlicher Rabin für x^(q^n_j) - x mit n_j = n / p_j
        rabin' f (n:ns) | g /= pKonst 1  = False
                        | otherwise = rabin' f ns
          where g  = --trace ("rabin: ggTP for f="++show f++" h'-pX="++show (p2Tup (h'-pX))) $ 
                      (ggTP f (h'-pX))
                h' = --trace ("x^"++show q++"^"++show n++" mod f = h'="++show (modMonom q n f )) $ 
                      modMonom q n f

-------------------------------------------------------------------------------
-- Helper


-- |Primfaktorzerlegung 
--  aus http://www.haskell.org/haskellwiki/99_questions/Solutions/35
factor :: Int -> [Int]
factor 1 = []
factor n = let divisors = dropWhile ((/= 0) . mod n) [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ factor $ div n prime



-- | Schnelles Modulo für Monome, d.h. berechnet
--   x^n mod f
modMonom :: (Show a, Num a, Eq a, Fractional a) => 
                                                Int -> Int -> Polynom a -> Polynom a
modMonom q d f  = modMonom' n f
  where n  = (toInteger q)^(toInteger d) 
        modMonom' n f 
               | n < (toInteger df)  
                           = --trace ("modMonom n="++show n++" n<df") $
                            pTupUnsave [(fromInteger n,1)]
               | even n    = --trace ("modMonom n="++show n++" n ger") $
                              g `modByP` f
               | otherwise = --trace ("modMonom n="++show n++" n unger") $
                            (multMonomP 1 g) `modByP` f
          where df = uDegP f
                m  = n `quot` 2
                g  = h*h
                h  = modMonom' m f
