--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.Factorization
-- Note        : Ein paar kleine Funktionen, um mit Faktorisierungen zu Arbeiten
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Factorization
  ( toFact, unFact
  , potFact, appFact, aggFact
  )where
import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials

import Data.List

--------------------------------------------------------------------------------
-- Eine Faktoriesierung wird dargestellt als Liste von Paaren (Int, Polynom a)
-- wobei ein Paar (i,f) den Faktor f^i repräsentiert.
--
-- Ein Faktoriesierungsalgorithmus ist also gegeben durch eine Funktion:
--      Polynom a -> [(Int,Polynom a)]

-- |Erzeugt eine triviale Faktoriesierung zu einem Polynom
toFact :: Polynom a -> [(Int,Polynom a)]
toFact f = [(1,f)]

-- |Multipliziert eine Faktoriesierung aus
unFact :: (FiniteField a, Num a, Fractional a) => [(Int,Polynom a)] -> Polynom a
unFact fs = product $ map (\(i,f) -> f^i) fs

-- |Ersetzt eine Faktoriesierung, durch die n-te Potenz dieser Faktoriesierung
potFact :: (FiniteField a, Num a, Fractional a) => Int -> [(Int,Polynom a)]
                                                           -> [(Int,Polynom a)]
potFact _ []         = []
potFact n ((i,f):ts) = (i*n,f) : potFact n ts

-- |Nimmt eine Faktoriesierung und wendet auf diese einen gegebenen
-- Faktoriesierungsalgorithmus an
appFact :: (FiniteField a, Num a, Fractional a) => 
  (Polynom a -> [(Int,Polynom a)]) -> [(Int,Polynom a)] -> [(Int,Polynom a)]
appFact _ []           = []
appFact alg ((i,f):ts) = potFact i (alg f) ++ appFact alg ts

-- |Fasst in einer Faktoriesierung gleiche Funktionen Zusammen
aggFact :: (FiniteField a, Num a, Fractional a) => [(Int,Polynom a)]
                                                            -> [(Int,Polynom a)]
aggFact l = [(sum [i | (i,g) <- l , f==g],f) | f <- nub [f | (_,f) <- l], f /= P[one]]

