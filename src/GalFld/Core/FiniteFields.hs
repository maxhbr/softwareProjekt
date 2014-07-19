--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Core.FiniteFields
-- Note        : Allgemeine implementierung endlicher Körper
--
--
--
--------------------------------------------------------------------------------
module GalFld.Core.FiniteFields
  ( FFElem (..), listFFElem
  , aggF
  , charOfP, charRootP
  , module X
  ) where
import Data.Maybe
import Data.List
import Control.Exception
import Data.Binary
import Control.Monad
import Data.Ord (Ordering (..))
import Control.DeepSeq

import GalFld.Core.FiniteField as X
import GalFld.Core.PrimeFields as X
import GalFld.Core.Polynomials
import GalFld.Core.ShowTex

import Debug.Trace

--------------------------------------------------------------------------------
--  Definition

-- Ein Element im Körper ist repräsentiert durch ein Paar von Polynomen. Das
-- erste beschreibt das Element, das zweite das Minimalpolynom
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
  (FFElem f p) == (FFKonst y)  = isNullP $ f - pKonst y
  (FFKonst x)  == (FFElem g p) = isNullP $ g - pKonst x
  (FFElem f p) == (FFElem g q) | p==q       = isNullP $ f-g
                              | otherwise = error "Not the same mod"

instance (Show a, Num a, Eq a) => Show (FFElem a) where
  show (FFKonst x)                = "(" ++ show x ++ " mod ...)"
  show (FFElem f p) | isNullP f   = "(0 mod " ++ show p ++ ")"
                    | otherwise   = "(" ++ show f ++ " mod " ++ show p ++")"

instance (ShowTex a, Num a, Eq a) => ShowTex (FFElem a) where
  showTex (FFKonst x) = showTex x
  showTex (FFElem f p)
    | isNullP f   = "\\left(\\underline{0}_{mod~" ++ showTex p ++ "}\\right)"
    | otherwise   =
      "\\left(\\underline{" ++ showTex f ++ "}_{mod~" ++ showTex p ++"}\\right)"


instance (Show a, Num a, Eq a, Fractional a) => Num (FFElem a) where
  fromInteger i                           = FFKonst (fromInteger i)
  {-# INLINE (+) #-}
  (FFKonst x)  + (FFKonst y)              = FFKonst (x+y)
  (FFElem f p) + (FFKonst x)              = FFElem (f + pKonst x) p
  (FFKonst x)  + (FFElem f p)             = FFElem (f + pKonst x) p
  (FFElem f p) + (FFElem g q) | p==q       = aggF $ FFElem (f+g) p
                              | otherwise = error "Not the same mod"
  {-# INLINE (*) #-}
  (FFKonst x)  * (FFKonst y)              = FFKonst (x*y)
  (FFElem f p) * (FFKonst x)              = FFElem (f * pKonst x) p
  (FFKonst x)  * (FFElem f p)             = FFElem (f * pKonst x) p
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
  recip (FFElem f p) | isNullP f     = error "Division by zero"
                     | otherwise     = FFElem s p
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

instance (Num a, Eq a, NFData a) => NFData (FFElem a) where
  rnf (FFElem f p) = rnf (f,p)
  rnf (FFKonst x)  = rnf x

-- |Nimmt ein Element aus einem Endlichen Körper und gibt eine Liste aller
-- anderen Elemente zurrück.
-- Diese Funktion benötigt ein FFElem, ein FFKonst ist zu universell und
-- enthält deswegen zu wenig Information, über den Körper in dem es lebt.
elems' :: (Show a, Num a, Fractional a, FiniteField a) => FFElem a -> [FFElem a]
elems' (FFKonst x)      = error "Insufficient information in FFKonst"
elems' elm@(FFElem f p) =
                map (`FFElem` p) (nullP : getAllP (elems e) (uDegP p -1))
  where e = getReprP p

{-# INLINE getReprP' #-}
getReprP' f = getReprP'' $ p2Tup f
getReprP'' []                   =
                            error "Insufficient information in this Polynomial"
getReprP'' ((i,FFKonst _):ms)   = getReprP'' ms
getReprP'' ((i,FFElem f p): ms) = FFElem 0 p

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
                _ -> trace (show t) undefined

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen über Endlichen Körpern

{-# INLINE charOfP #-}
-- |Gibt die Charakteristik der Koeffizienten eines Polynoms
charOfP :: (Eq a, FiniteField a, Num a) => Polynom a -> Int
charOfP f = charakteristik $ getReprP f

{-# INLINE charRootP #-}
-- |Zieht die p-te Wurzel aus einem Polynom, wobei p die Charakteristik ist
charRootP :: (Show a, FiniteField a, Num a) => Polynom a -> Polynom a
charRootP f | isNullP f     = nullP
            | f == pKonst 1  = pKonst 1
            | otherwise     = pTupUnsave [(i `quot` p,m^l) | (i,m) <- p2Tup f]
  where p = charOfP f
        q = elemCount $ getReprP f
        l = max (quot q p) 1

hasNSInFF :: (Eq a, Num a, FiniteField a) => Polynom a -> Bool
hasNSInFF f = not (null [f | e <- elems (getReprP f), evalP e f == 0])
