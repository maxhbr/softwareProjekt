--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Core.Factorization
-- Note        : Ein paar kleine Funktionen, um mit Faktorisierungen zu Arbeiten
--
-- Eine Faktorisierung wird dargestellt als Liste von Paaren (Int, Polynom a)
-- wobei ein Paar (i,f) den Faktor f^i repräsentiert.
--
-- Ein Faktorisierungsalgorithmus ist also gegeben durch eine Funktion:
--      Polynom a -> [(Int,Polynom a)]

--------------------------------------------------------------------------------
module GalFld.Core.Factorization
  ( toFact, unFact
  , potFact, appFact, aggFact
  , isTrivialFact
  -- Faktorisierungen
  , obviousFactor, appObFact, findTrivialsOb
  , findTrivialsNs
  )where
import Data.List
import Control.Parallel
import Control.Parallel.Strategies
import Control.Arrow as A

import GalFld.Core.FiniteFields
import GalFld.Core.Polynomials
--------------------------------------------------------------------------------

-- |Erzeugt eine triviale Faktorisierung zu einem Polynom
toFact :: Polynom a -> [(Int,Polynom a)]
toFact f = [(1,f)]

-- |Multipliziert eine Faktorisierung aus
unFact :: (FiniteField a, Num a, Fractional a) => [(Int,Polynom a)] -> Polynom a
unFact []      = pKonst 1
unFact [(1,f)] = f
unFact [(i,f)] = f^i
unFact fs      = product $ map (\(i,f) -> f^i) fs

-- |Ersetzt eine Faktorisierung, durch die n-te Potenz dieser Faktorisierung
potFact :: (Num a) => Int -> [(Int,Polynom a)] -> [(Int,Polynom a)]
potFact _ []         = []
potFact n ((i,f):ts) = (i*n,f) : potFact n ts

-- |Nimmt eine Faktorisierung und wendet auf diese einen gegebenen
-- Faktorisierungsalgorithmus an
appFact :: (Eq a, Num a) =>
  (Polynom a -> [(Int,Polynom a)]) -> [(Int,Polynom a)] -> [(Int,Polynom a)]
appFact alg = withStrategy (parList rpar) . concatMap
  (uncurry appFact')
  where appFact' i f  | isNullP f   = [(i,nullP)]
                      | uDegP f <= 1 = [(i,f)]
                      | otherwise   = potFact i (alg f)

-- |Fasst in einer Faktorisierung gleiche Faktoren zusammen
aggFact :: (Num a, Eq a) => [(Int,Polynom a)] -> [(Int,Polynom a)]
aggFact l = [(sum [i | (i,g) <- l , f==g],f) | f <- nub [f | (_,f) <- l], 
                                                                f /= pKonst 1]

-- |Sagt, ob die gegebene Faktorisierung trivial ist, also aus nur einem
-- echten Faktor besteht
isTrivialFact :: (Num a, Eq a) => [(Int,Polynom a)] -> Bool
isTrivialFact [] = error "A unit has no welldefined factorization"
isTrivialFact [(1,_)] = True
isTrivialFact ms = sum (map fst ms') == 1
  where ms' = filter (\(_,f) -> f/=1) ms

-- |Gibt alle Faktorisierungen zurück, welche nach der offensichtlichen
-- Faktorisierung noch trivial sind
findTrivialsOb :: (Show a, Fractional a, Num a, FiniteField a) => 
                                          [Polynom a] -> [[(Int,Polynom a)]]
findTrivialsOb ps = [fs | fs <- parMap rpar appObFact
                     [toFact f | f <- ps , not (isNullP f)], isTrivialFact fs]

-- |Gibt alle Polynome zurück, die keine Nullstellen haben.
findTrivialsNs :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a]
  -> [[(Int,Polynom a)]]
findTrivialsNs ps = [toFact f | f <- ps, not (hasNs f es) || uDegP f < 2]
  where es = elems $ getReprP $ head ps


--------------------------------------------------------------------------------
--  Einfache / offensichtliche Faktorisierungen

obviousFactor :: (Show a, Num a, Eq a) => Polynom a -> [(Int,Polynom a)]
obviousFactor f | isNullP f     = [(1,nullP)]
                | uDegP f <= 1   = [(1,f)]
                | hasNoKonst fs = factorX
                | otherwise     = toFact f
  where fs = p2Tup f
        -- Teste, ob ein konstanter Term vorhanden ist
        hasNoKonst ms | fst (last ms) == 0  = False
                      | otherwise          = True
        -- Hier kann man d mal X ausklammern
        factorX | g == 1     = [(d,pTupUnsave [(1,1)])]
                | otherwise = [(d,pTupUnsave [(1,1)]), (1,g)]
          where d = fst $ last fs
                g = pTupUnsave $ map (A.first (\x -> x-d)) fs

appObFact :: (Show a, Num a, Eq a) => [(Int,Polynom a)] -> [(Int,Polynom a)]
appObFact = appFact obviousFactor
