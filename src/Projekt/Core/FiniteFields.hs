--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.FiniteFields
-- Note        : Allgemeine implementierung endlicher Körper
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.FiniteFields
  (
  ) where
import Projekt.Core.FiniteField
import Projekt.Core.PrimeFields
import Projekt.Core.Polynomials

data FFElem a = FFElem (Polynom a) (Polynom a) | FFKonst a

konstToElem :: (Num a, Eq a) => FFElem a -> Polynom a -> FFElem a
konstToElem (FFKonst a) q              = FFElem (P [(0,a)]) q
konstToElem (FFElem f p) q | p==q       = FFElem f p
                           | otherwise = error "Not the same mod"

instance Eq (FFElem a) where
  x == y = undefined

instance (Show a, Eq a) => Show (FFElem a) where
  show (FFElem x y) = show x ++ " mod " ++ show y

instance (Num a, Eq a, Fractional a) => Num (FFElem a) where
  fromInteger i                           = FFKonst (fromInteger i)
  (FFKonst x) + (FFKonst y)               = FFKonst (x+y)
  (FFElem f p) + (FFKonst x)              = FFElem (modByP (f+P [(0,x)]) p) p
  -- ...
  (FFElem f p) + (FFElem g q) | p==q       = FFElem (modByP (f+g) p) p
                              | otherwise = error "Not the same mod"
  (FFElem f p) * (FFElem g q) | p==q       = FFElem (modByP (f*g) p) p
                              | otherwise = error "Not the same mod"
  negate (FFElem f p)                     = FFElem (negate f) p
  abs _                                   = error "Prelude.Num.abs: inappropriate abstraction"
  signum _                                = error "Prelude.Num.signum: inappropriate abstraction"

instance (Eq a, Fractional a) => Fractional (FFElem a) where
  fromRational _     = error "inappropriate abstraction"
  recip (FFElem f p) = FFElem s p
    where (d,s,t) = eekP f p

instance (FiniteField a) => FiniteField (FFElem a) where
  one  = FFKonst one
  zero = FFKonst zero
  elems = undefined
  units = undefined

--------------------------------------------------------------------------------
--  Beispiel

{- F4=E2 als Grad 2 Erweiterung von Z2
 -
 - Irreduzibles Polynom von Grad 2 über Z2:
 -           x²+x+1
 - Mit einer Nullstelle:
 -           v
 -
 - Also ist F4=Z2(v)
 -
 - Tabellen:
 -           +   | 0   | 1   | v   | v+1                 *   | 1   | v   | v+1
 -           ----+-----+-----+-----+-----                ----+-----+-----+-----
 -           0   | 0   | 1   | v   | v+1                 1   | 1   | v   | v+1
 -           ----+-----+-----+-----+-----                ----+-----+-----+-----
 -           1   | 1   | 0   | v+1 | v                   v   | v   | v+1 | 1
 -           ----+-----+-----+-----+-----                ----+-----+-----+-----
 -           v   | v   | v+1 | 0   | 1                   v+1 | v+1 | 1   | v
 -           ----+-----+-----+-----+-----
 -           v+1 | v+1 | v   | 1   | 0
 -}

{- F8=E4 als Grad 4 Erweiterung von Z2
 - durck MPol x⁴+x²+1
 -
 - oder
 - als Grad 2 Erweiterung con E2 durch MPol x²+x+1
 -}


--------------------------------------------------------------------------------
--  Weiteres Beispiel

{-
 - Z[X,Y,Z]/
 -        /                                  = GF(3²⁷) = F3^27
 -       /ideal(3,x³-x-1,y³-y+x²,z³-z+x²y²)
 -}
