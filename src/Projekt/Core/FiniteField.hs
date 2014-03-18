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

class (Eq a) => FiniteField a where
  zero, one    :: a
  elems, units :: [a]
