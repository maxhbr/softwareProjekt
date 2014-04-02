{-# LANGUAGE CPP #-}
#define LATEXFORMAT
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
  , aggF
  , module X
  ) where
import Projekt.Core.FiniteField as X
import Projekt.Core.PrimeFields as X
import Projekt.Core.Polynomials

-- Ein Element im Körper ist repräsentiert durch ein Paar von Polynomen. Das
-- erste beschreibt das Element und das zweite beschreibt das Minimalpolynom
-- und damit den Erweiterungskörper.
-- Zusätzlich ist auch die kanonische Inklusion aus dem Grundkörper durch
-- FFKonst implementiert.
data FFElem a = FFElem (Polynom a) (Polynom a) | FFKonst a

aggF :: (Eq a, Fractional a) => FFElem a -> FFElem a
aggF (FFKonst x)  = FFKonst x
aggF (FFElem f p) = FFElem (modByP f p) p

instance (Num a, Eq a, Fractional a) => Eq (FFElem a) where
  (FFKonst x)  == (FFKonst y)  = x==y
  (FFElem f p) == (FFKonst y)  = null $ unP $ aggP (f - P [(0,y)])
  (FFKonst x)  == (FFElem g p) = null $ unP $ aggP (P [(0,x)] - g)
  (FFElem f p) == (FFElem g q) | p==q       = null $ unP $ aggP (f - g)
                              | otherwise = error "Not the same mod"

instance (Show a, Eq a) => Show (FFElem a) where
  show (FFKonst x)       = show x
#ifdef LATEXFORMAT
  show (FFElem (P []) p) = "\\left(\\underline{0}~mod~" ++ show p ++ "\\right)"
  show (FFElem f p)      = "\\left(\\underline{" ++ show f ++ "}~mod~" ++ show p ++"\\right)"
#else 
  show (FFElem (P []) p) = "(0 mod " ++ show p ++ ")"
  show (FFElem f p)      = "(" ++ show f ++ " mod " ++ show p ++")"
#endif

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

-- |Nimmt ein Element aus einem Endlichen Körper und gibt eine Liste aller
-- anderen Elemente zurrück.
-- Diese Funktion benötigt ein FFElem, ein FFKonst ist zu universell und
-- enthält deswegen zu wenig Information, über den Körper in dem es lebt.
elems' :: (Num a, Fractional a, FiniteField a) => FFElem a -> [FFElem a]
elems' (FFKonst x)  = error "Insufficient information in FFKonst"
elems' (FFElem f p) = map (`FFElem` p) (getAllP (elems exmp) deg)
  where deg  = degP p
        exmp = product (map snd (unP f)) * product (map snd (unP p))
