--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Algorithmen.Factorizations
-- Note        : Fasst Faktorisierungsalgorithmen zusammen
--
--------------------------------------------------------------------------------
module GalFld.Algorithmen.Factorizations (
  factorP
  ) where
import GalFld.Core
import GalFld.Algorithmen.Berlekamp
import GalFld.Algorithmen.SFreeFactorization

--------------------------------------------------------------------------------

-- |Faktorisiere vollständig
factorP :: (Show a, Num a, FiniteField a, Fractional a, Eq a)
                                            => Polynom a -> [(Int,Polynom a)]
factorP = appBerlekamp . appSff . obviousFactor

