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
  , isTrivialFact
  -- Faktoriesierungen
  , obviousFactor, appObFact, findTrivialsOb
  )where
import Data.List

import Control.Parallel
import Control.Parallel.Strategies

import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials

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
unFact [(1,f)] = f
unFact [(i,f)] = f^i
unFact fs      = product $ map (\(i,f) -> f^i) fs

-- |Ersetzt eine Faktoriesierung, durch die n-te Potenz dieser Faktoriesierung
potFact :: (Num a) => Int -> [(Int,Polynom a)]
                                                           -> [(Int,Polynom a)]
potFact _ []         = []
potFact n ((i,f):ts) = (i*n,f) : potFact n ts

-- |Nimmt eine Faktoriesierung und wendet auf diese einen gegebenen
-- Faktoriesierungsalgorithmus an
appFact :: (Num a) =>
  (Polynom a -> [(Int,Polynom a)]) -> [(Int,Polynom a)] -> [(Int,Polynom a)]
appFact alg = withStrategy (parList rpar) . concatMap
  (\(i,f) -> potFact i (alg f))

-- |Fasst in einer Faktoriesierung gleiche Funktionen Zusammen
aggFact :: (Num a, Eq a) => [(Int,Polynom a)]
                                                            -> [(Int,Polynom a)]
aggFact l = [(sum [i | (i,g) <- l , f==g],f) | f <- nub [f | (_,f) <- l], f /= P[1]]

isTrivialFact :: [(Int,a)] -> Bool
isTrivialFact [] = error "[] is not a factorization"
isTrivialFact [(1,_)] > True
isTrivialFact ms = sum (map fst ms) == 1

-- |Gibt alle Faktorisierungen zurück, welche nach der offensichtlichen
-- Faktoriesierung noch trivial sind
findTrivialsOb :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [[(Int,Polynom a)]]
findTrivialsOb ps = [fs | fs <- parMap rpar appObFact
                        [(toFact . aggP) f | f <- ps , f /= P[]]
                      , isTrivialFact fs]

--------------------------------------------------------------------------------
--  Einfache / offensichtliche Faktorisierungen

obviousFactor :: (Num a, Eq a) => Polynom a -> [(Int,Polynom a)]
obviousFactor (P[])      = [(1,P[])]
obviousFactor (P[m])     = [(1,P[m])]
obviousFactor (P[m0,m1]) = [(1,P[m0,m1])]
obviousFactor (P (0:ms)) = aggFact $ (1,P[0,1]) : obviousFactor (P ms)
obviousFactor f          = toFact f

appObFact :: (Num a, Eq a) => [(Int,Polynom a)] -> [(Int,Polynom a)]
appObFact = appFact obviousFactor
