--------------------------------------------------------------------------------
-- | 
-- Module      : Project.Core.FiniteField
-- Note        : Finite field class
-- 
-- 
-- 
--------------------------------------------------------------------------------

module Projekt.Core.FiniteField
  ( FiniteField(..)
  ) where

class (Eq a) => FiniteField a where
  zero, one    :: a
  elems, units :: a -> [a]

  units x = [e | e <- elems x, e /= zero]
