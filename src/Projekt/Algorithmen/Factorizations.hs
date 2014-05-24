--------------------------------------------------------------------------------
-- |
-- Module      : Project.Algorithmen.Factorizations
-- Note        : Fasst Faktorisierungsalgorithmen zusammen
--------------------------------------------------------------------------------
module Projekt.Algorithmen.Factorizations (
factorP
)	where
import Projekt.Core
import Projekt.Algorithmen.Berlekamp
import Projekt.Algorithmen.SFreeFactorization

--------------------------------------------------------------------------------

-- |Faktorisiere vollständig
factorP :: (Show a, Num a, FiniteField a, Fractional a, Eq a)   
                                            => Polynom a -> [(Int,Polynom a)]
factorP = appBerlekamp . appSff . aggFact . obviousFactor

