--------------------------------------------------------------------------------
-- | 
-- Module      : Projekt.Core.LatexShow
-- Note        : 
-- 
-- 
-- 
--------------------------------------------------------------------------------

module Projekt.Core.LatexShow
  ( LatexShow(..)
  ) where

class LatexShow a where
  latexShow :: a -> String
