--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.FiniteFields
-- Note        : Allgemeine implementierung endlicher Körper
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.FiniteFields
  ( FFElem (..), listFFElem
  , aggF
  , charOfP, charRootP
  , module X
  ) where
import Data.Maybe
import Control.Exception

import Projekt.Core.FiniteField as X
import Projekt.Core.PrimeFields as X
import Projekt.Core.Polynomials
import Projekt.Core.ShowTex

--------------------------------------------------------------------------------
--  Definition

-- Ein Element im Körper ist repräsentiert durch ein Paar von Polynomen. Das
-- erste beschreibt das Element und das zweite beschreibt das Minimalpolynom
-- und damit den Erweiterungskörper.
-- Zusätzlich ist auch die kanonische Inklusion aus dem Grundkörper durch
-- FFKonst implementiert.
data FFElem a = FFElem (Polynom a) (Polynom a) | FFKonst a

aggF :: (Eq a, Fractional a) => FFElem a -> FFElem a
aggF (FFKonst x)  = FFKonst x
aggF (FFElem f p) = FFElem (modByP f p) p

listFFElem m = map (`FFElem` m)

--------------------------------------------------------------------------------
--  Instanzen

instance (Num a, Eq a, Fractional a) => Eq (FFElem a) where
  (FFKonst x)  == (FFKonst y)  = x==y
  (FFElem f p) == (FFKonst y)  = null $ unP $ aggP (f - P[y])
  (FFKonst x)  == (FFElem g p) = null $ unP $ aggP (P[x] - g)
  (FFElem f p) == (FFElem g q) | p==q       = null $ unP $ aggP (f - g)
                              | otherwise = error "Not the same mod"

instance (Show a, Num a, Eq a) => Show (FFElem a) where
  show (FFKonst x)       = "(" ++ show x ++ " mod ...)"
  show (FFElem (P []) p) = "(0 mod " ++ show p ++ ")"
  show (FFElem f p)      = "(" ++ show f ++ " mod " ++ show p ++")"

instance (ShowTex a, Num a, Eq a) => ShowTex (FFElem a) where
  showTex (FFKonst x)       = showTex x
  showTex (FFElem (P []) p) =
    --"\\left(\\underline{0}_{mod~" ++ showTex p ++ "}\\right)"
    "\\left(\\underline{0}_{mod~" ++ showTex p ++ "}\\right)"
  showTex (FFElem f p)      =
    --"\\left(\\underline{" ++ showTex f ++ "}_{mod~" ++ showTex p ++"}\\right)"
    "\\left(\\underline{" ++ showTex f ++ "}_{mod~" ++ showTex p ++"}\\right)"


instance (Num a, Eq a, Fractional a) => Num (FFElem a) where
  fromInteger i                           = FFKonst (fromInteger i)

  (FFKonst x)  + (FFKonst y)              = FFKonst (x+y)
  (FFElem f p) + (FFKonst x)              = FFElem (f+P[x]) p
  (FFKonst x)  + (FFElem f p)             = FFElem (f+P[x]) p
  (FFElem f p) + (FFElem g q) | p==q       = aggF $ FFElem (f+g) p
                              | otherwise = error "Not the same mod"

  (FFKonst x)  * (FFKonst y)              = FFKonst (x*y)
  (FFElem f p) * (FFKonst x)              = FFElem (f*P [x]) p
  (FFKonst x)  * (FFElem f p)             = FFElem (f*P [x]) p
  (FFElem f p) * (FFElem g q) | p==q       = aggF $ FFElem (f*g) p
                              | otherwise = error "Not the same mod"

  negate (FFKonst x)                      = FFKonst (negate x)
  negate (FFElem f p)                     = FFElem (negate f) p

  abs _    = error "Prelude.Num.abs: inappropriate abstraction"
  signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance (Show a, Eq a, Fractional a) => Fractional (FFElem a) where
  fromRational _     = error "inappropriate abstraction"
  recip (FFKonst x)  = FFKonst (recip x)
  recip (FFElem f p) | FFElem f p == FFElem (P []) p = error "Division by zero"
                     | otherwise                    = FFElem s p
    where (_,s,_) = eekP f p

instance (Num a, Fractional a, FiniteField a) => FiniteField (FFElem a) where
  zero                             = FFKonst zero
  one                              = FFKonst one
  elems                            = elems'
  charakteristik (FFElem (P ms) _) = charakteristik $ product ms
  charakteristik (FFKonst x)       = charakteristik x
  {-elemCount                        = length . elems'-}
  elemCount (FFKonst _)            = error "Insufficient information in FFKonst"
  elemCount (FFElem (P ms) m)      = elemCount (product ms) ^ uDegP m

-- |Nimmt ein Element aus einem Endlichen Körper und gibt eine Liste aller
-- anderen Elemente zurrück.
-- Diese Funktion benötigt ein FFElem, ein FFKonst ist zu universell und
-- enthält deswegen zu wenig Information, über den Körper in dem es lebt.
elems' :: (Num a, Fractional a, FiniteField a) => FFElem a -> [FFElem a]
elems' (FFKonst x)  = error "Insufficient information in FFKonst"
elems' (FFElem f p) = map (`FFElem` p) (getAllP (elems fieldElem) deg)
  where deg  = fromJust $ degP p
        fieldElem = product (unP f) * product (unP p)

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen über Endlichen Körpern

-- |Gibt die Charakteristik der Koeffizienten eines Polynoms
-- TODO: Product sollte nicht nötig sein!
--       Möglicherweise mit Backtracking
charOfP :: (FiniteField a, Num a) => Polynom a -> Int
charOfP (P ms) = charakteristik $ product ms 

-- |Zieht die p-te wurzel aus einem Polynom, wobei p die charakteristik ist
charRootP :: (FiniteField a, Num a) => Polynom a -> Polynom a
charRootP (P ms)= P[m^l | (m,i) <- zip ms [0..] , i `mod` p == 0]
  where p = charOfP $ P ms
        -- TODO: Das folgende ist unglaublich langsam!
        q = elemCount $ product ms
        l = max (q-p) 1
