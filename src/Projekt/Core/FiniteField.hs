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

class (Eq a, Num a) => FiniteField a where
  zero, one    :: a
  inv          :: a -> a
  (/)          :: a -> a -> a
  elems, units :: [a]
