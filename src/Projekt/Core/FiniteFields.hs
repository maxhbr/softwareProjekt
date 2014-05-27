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
import Data.Binary
import Control.Monad

import Projekt.Core.FiniteField as X
import Projekt.Core.PrimeFields as X
import Projekt.Core.Polynomials
import Projekt.Core.ShowTex

import Debug.Trace

--------------------------------------------------------------------------------
--  Definition

-- Ein Element im Körper ist repräsentiert durch ein Paar von Polynomen. Das
-- erste beschreibt das Element und das zweite beschreibt das Minimalpolynom
-- und damit den Erweiterungskörper.
-- Zusätzlich ist auch die kanonische Inklusion aus dem Grundkörper durch
-- FFKonst implementiert.
data FFElem a = FFElem (Polynom a) (Polynom a) | FFKonst a

aggF :: (Show a, Eq a, Fractional a) => FFElem a -> FFElem a
aggF (FFKonst x)  = FFKonst x
aggF (FFElem f p) = FFElem (modByP f p) p

listFFElem m = map (`FFElem` m)

--------------------------------------------------------------------------------
--  Instanzen

instance (Show a, Num a, Eq a, Fractional a) => Eq (FFElem a) where
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


instance (Show a, Num a, Eq a, Fractional a) => Num (FFElem a) where
  fromInteger i                           = FFKonst (fromInteger i)

  {-# INLINE (+) #-}
  (FFKonst x)  + (FFKonst y)              = FFKonst (x+y)
  (FFElem f p) + (FFKonst x)              = FFElem (f+P[x]) p
  (FFKonst x)  + (FFElem f p)             = FFElem (f+P[x]) p
  (FFElem f p) + (FFElem g q) | p==q       = aggF $ FFElem (f+g) p
                              | otherwise = error "Not the same mod"

  {-# INLINE (*) #-}
  (FFKonst x)  * (FFKonst y)              = FFKonst (x*y)
  (FFElem f p) * (FFKonst x)              = FFElem (f*P [x]) p
  (FFKonst x)  * (FFElem f p)             = FFElem (f*P [x]) p
  (FFElem f p) * (FFElem g q) | p==q       = aggF $ FFElem (f*g) p
                              | otherwise = error "Not the same mod"

  {-# INLINE negate #-}
  negate (FFKonst x)                      = FFKonst (negate x)
  negate (FFElem f p)                     = FFElem (negate f) p

  abs _    = error "Prelude.Num.abs: inappropriate abstraction"
  signum _ = error "Prelude.Num.signum: inappropriate abstraction"

instance (Show a, Eq a, Fractional a) => Fractional (FFElem a) where
  fromRational _     = error "inappropriate abstraction"
  {-# INLINE recip #-}
  recip (FFKonst x)  = FFKonst (recip x)
  recip (FFElem f p) | FFElem f p == FFElem (P []) p = error "Division by zero"
                     | otherwise                    = FFElem s p
    where (_,s,_) = eekP f p

instance (Show a, Eq a, Num a, Fractional a, FiniteField a) => FiniteField (FFElem a) where
  zero                        = FFKonst zero
  one                         = FFKonst one
  elems                       = elems'
  {-# INLINE charakteristik #-}
  charakteristik (FFElem _ m) = charakteristik $ getReprP m
  charakteristik (FFKonst x)  = charakteristik x
  elemCount (FFKonst _)       = error "Insufficient information in FFKonst"
  elemCount (FFElem _ m)      = elemCount (getReprP m) ^ uDegP m
  {-# INLINE getReprP #-}
  getReprP                    = getReprP'

-- |Nimmt ein Element aus einem Endlichen Körper und gibt eine Liste aller
-- anderen Elemente zurrück.
-- Diese Funktion benötigt ein FFElem, ein FFKonst ist zu universell und
-- enthält deswegen zu wenig Information, über den Körper in dem es lebt.
elems' :: (Show a, Num a, Fractional a, FiniteField a) => FFElem a -> [FFElem a]
elems' (FFKonst x)  = error "Insufficient information in FFKonst"
elems' elm@(FFElem f p) = map (`FFElem` p) (P[] : getAllP (elems e) (uDegP p -1))
  where e = getReprP p

{-# INLINE getReprP' #-}
getReprP' (P [])                = error "Insufficient information in this Polynomial"
getReprP' (P (FFKonst _:ms))    = getReprP' $ P ms
getReprP' (P (FFElem f p : ms)) = FFElem 0 p

instance (Num a, Binary a) => Binary (FFElem a) where
  put (FFKonst f)  = do put (0 :: Word8)
                        put f
  put (FFElem f p) = do put (1 :: Word8)
                        put f
                        put p

  get = do t <- get :: Get Word8
           case t of
                0 -> liftM FFKonst get
                1 -> liftM2 FFElem get get

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen über Endlichen Körpern


{-# INLINE charOfP #-}
-- |Gibt die Charakteristik der Koeffizienten eines Polynoms
charOfP :: (Eq a, FiniteField a, Num a) => Polynom a -> Int
charOfP f = charakteristik $ getReprP f

{-# INLINE charRootP #-}
-- |Zieht die p-te wurzel aus einem Polynom, wobei p die charakteristik ist
charRootP :: (FiniteField a, Num a) => Polynom a -> Polynom a
charRootP (P []) = P []
charRootP (P [1]) = P [1]
charRootP (P ms) = P[m^l | (m,i) <- zip ms [0..] , i `mod` p == 0]
  where p = charOfP $ P ms
        q = elemCount $ getReprP (P ms)
        l = max (quot q p) 1

hasNSInFF :: (Eq a, Num a, FiniteField a) => Polynom a -> Bool
hasNSInFF f = not (null [f | e <- elems (getReprP f), evalP e f == 0])
