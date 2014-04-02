--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.FiniteFields
-- Note        : Allgemeine implementierung endlicher Körper
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.FiniteFields
  ( FFElem (..)
  , konstToElem
  , aggF
  , module X
  ) where
import Projekt.Core.FiniteField as X
import Projekt.Core.PrimeFields as X
import Projekt.Core.Polynomials
--import Debug.Trace

data FFElem a = FFElem (Polynom a) (Polynom a) | FFKonst a

konstToElem :: (Num a, Eq a) => FFElem a -> Polynom a -> FFElem a
konstToElem (FFKonst a) q              = FFElem (P [(0,a)]) q
konstToElem (FFElem f p) q | p==q       = FFElem f p
                           | otherwise = error "Not the same mod"

aggF :: (Eq a, Fractional a) => FFElem a -> FFElem a
aggF (FFKonst x)  = FFKonst x
aggF (FFElem f p) = FFElem (modByP f p) p

instance (Num a, Eq a, Fractional a) => Eq (FFElem a) where
  (FFKonst x)  == (FFKonst y)  = x==y
  (FFElem f p) == (FFKonst y)  = FFElem f p == konstToElem (FFKonst y) p
  (FFKonst x)  == (FFElem g p) = konstToElem (FFKonst x) p == FFElem g p
  (FFElem f p) == (FFElem g q) | p==q       = null $ unP $ aggP (f - g)
                              | otherwise = error "Not the same mod"

instance (Show a, Eq a) => Show (FFElem a) where
  show (FFKonst x)       = show x
  show (FFElem (P []) p) = "\x1B[01m0\x1B[00m mod " ++ show p
  show (FFElem f p)      = "\x1B[01m" ++ show f ++ "\x1B[22m mod " ++ show p

instance (Num a, Eq a, Fractional a) => Num (FFElem a) where
  fromInteger i                           = FFKonst (fromInteger i)

  (FFKonst x)  + (FFKonst y)              = FFKonst (x+y)
  (FFElem f p) + (FFKonst x)              = FFElem (f+P [(0,x)]) p
  (FFKonst x)  + (FFElem f p)             = FFElem (f+P [(0,x)]) p
  (FFElem f p) + (FFElem g q) | p==q       = aggF $ FFElem (f+g) p
                              | otherwise = error "Not the same mod"

  (FFKonst x)  * (FFKonst y)              = FFKonst (x*y)
  (FFElem f p) * (FFKonst x)              = FFElem (f*P [(0,x)]) p
  (FFKonst x)  * (FFElem f p)             = FFElem (f*P [(0,x)]) p
  (FFElem f p) * (FFElem g q) | p==q       = aggF $ FFElem (f*g) p
                              | otherwise = error "Not the same mod"

  negate (FFKonst x)                      = FFKonst (negate x)
  negate (FFElem f p)                     = FFElem (negate f) p

  abs _    = error "Prelude.Num.abs: inappropriate abstraction"
  signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance (Show a, Eq a, Fractional a) => Fractional (FFElem a) where
  fromRational _     = error "inappropriate abstraction"
  recip (FFKonst x)  = FFKonst (recip x)
  recip (FFElem f p) = FFElem s p
    where (d,s,t) = eekP f p

instance (Num a, Fractional a, FiniteField a) => FiniteField (FFElem a) where
  zero  = FFKonst zero
  one   = FFKonst one
  elems = elems'

--------------------------------------------------------------------------------
--  TODO:
--  * elems

elems' :: (Num a, Fractional a, FiniteField a) => FFElem a -> [FFElem a]
elems' (FFKonst x)  = error "Not enougth information in FFKonst"
elems' (FFElem f p) = map (`FFElem` p) (getAllP (elems exmp) deg)
  where deg  = degP p
        exmp = product (map snd (unP f)) * product (map snd (unP p))
