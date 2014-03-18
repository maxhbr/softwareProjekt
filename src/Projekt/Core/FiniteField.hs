--------------------------------------------------------------------------------
-- | 
-- Module      : Project.Core.FiniteField
-- Note        : Finite field class
-- 
-- 
-- 
--------------------------------------------------------------------------------

module Projekt.Core.FiniteField
  {-( FiniteField )-}
  where
import Projekt.Core.Polynomials

{-
import Prelude hiding (fromInteger, negate, (+), (-), (*), (/))

class (Eq a) => FiniteField a where
  zero, one          :: a
  characteristik     :: Integer
  fromInteger        :: Integer -> a
  negate             :: a -> a
  (+), (-), (*), (/) :: a -> a -> a
  elems, units       :: [a]
 -}

class (Eq a) => FiniteField a where
  zero, one    :: a
  elems, units :: [a]


data FFElem a = FFElem (Polynom a) (Polynom a) | FFKonst a


instance (Show a, Eq a) => Show (FFElem a) where
  show (FFElem x y) = show x ++ " mod " ++ show y

instance (Num a, Eq a, Fractional a) => Num (FFElem a) where
  fromInteger                             = FFKonst fromInteger
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
  recip (FFElem f p)  =  FFElem s p
    where (d,s,t) = eekP f p
  fromRational _ = error "inappropriate abstraction"
