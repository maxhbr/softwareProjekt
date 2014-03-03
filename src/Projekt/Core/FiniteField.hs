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
{-
 - TODO:
 - * Erweiterungen von Körpern
 -}
