--------------------------------------------------------------------------------
-- | 
-- Module      : Project.Core.FiniteField
-- Note        : Finite field class
-- 
-- 
-- 
--------------------------------------------------------------------------------

module Projekt.Core.FiniteField
  -- Data
  (-- FFElem
  )
  where
import Projekt.Core.Polynomials

class (Eq a) => FiniteField a where
  zero, one    :: a
  elems, units :: [a]


--data FFElem a = FFElem (Polynom a) (Polynom a)



