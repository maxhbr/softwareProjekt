--------------------------------------------------------------------------------
-- |
-- Module      : Project.Algorithmen.Berlekamp
-- Note        : Implementiert eine Berlekamp Faktorisierung
--
-- Funktioniert nur auf Quadratfreien Polynomen
--
--------------------------------------------------------------------------------
module Projekt.Algorithmen.Berlekamp
  ( berlekamp, appBerlekamp, sffAndBerlekamp
  , findIrred, findIrreds, findTrivialsB
  -- Algorithmus
  , berlekampBasis
  , berlekampFactor
  )where
import Data.Maybe
import Data.List
import Debug.Trace

import Control.Parallel
import Control.Parallel.Strategies

import Projekt.Core
import Projekt.Algorithmen.SFreeFactorization

--------------------------------------------------------------------------------
--  Wrapper

berlekamp :: (Show a, FiniteField a, Num a, Fractional a) => Polynom a -> [(Int,Polynom a)]
berlekamp = appFact berlekampFactor . obviousFactor

appBerlekamp :: (Show a, FiniteField a, Num a, Fractional a) => [(Int,Polynom a)] -> [(Int,Polynom a)]
appBerlekamp = appFact berlekamp

-- |Faktorisiert ein Polynom f über einem endlichen Körper
-- Benutzt wird dazu die Quadratfreie Faktorisierung mit anschließendem
-- Berlekamp
sffAndBerlekamp :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
sffAndBerlekamp f = appBerlekamp $ sff f

-- |Wählt aus einer Liste von Polynomen das erste Irreduzibele Polynom heraus
findIrred :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> Polynom a
findIrred = head . findIrreds

-- |Filtert mittels SFF und Berlekamp aus einer Liste die irreduzibleneiner
-- liste heraus
findIrreds :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [Polynom a]
findIrreds (f:fs) = findIrreds' (f:fs)
  where findIrreds' []     = []
        findIrreds' (f:fs) | (not (hasNs f es)|| uDegP f < 2) &&
                             isTrivialFact fSff &&
                             isTrivialFact fB = f : findIrreds' fs
                           | otherwise = findIrreds' fs
          where fSff = appSff $ toFact f
                fB   = appBerlekamp fSff
        es = elems $ getReprP f

{-findIrreds ps = parMap rpar unFact $ findTrivialsB ps-}

-- |Gibt alle Faktorisierungen zurück, welche nach Berlekamp noch trivial sind
-- Wendet zuvor (die offensichtliche Faktorisierung und) SFF an
findTrivialsB :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [[(Int,Polynom a)]]
findTrivialsB ps = [fs | fs <- parMap rpar appBerlekamp
                       (findTrivialsSff ps)
                     , isTrivialFact fs]



--------------------------------------------------------------------------------
--  Algorithmus

-- |Berechnet eine Basis des Berlekampraums zu f,
--  d.h. gibt eine Matrix zurück, deren Zeilen gerade den Berlekampraum
--  aufspannen bzgl der kanonischen Basis { 1, x, x², x³, ... }
berlekampBasis :: (Show a, Fractional a, Num a, FiniteField a)
                                                       => Polynom a -> Matrix a
berlekampBasis f = transposeM $ kernelM $ transposeM $
                        fromListsM [red i | i <- [0..(n-1)]] - genDiagM 1 n
  where n     = fromJust $ degP f
        q     = elemCount a
        a     = getReprP f
        red i = takeFill 0 n $ p2List $ modByP (pTupUnsave [(i*q,1)]) f

-- |Faktorisiert ein Polynom f über einem endlichen Körper
--  Voraussetzungen: f ist quadratfrei
--  Ausgabe: Liste von irreduziblen, pw teilerfremden Polynomen
berlekampFactor :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
berlekampFactor f 
    | length triv == 1 = triv
    | length triv == 2 = (head triv) : (doBerlekamp $ snd $ last triv)
    | otherwise       = error "obviousFactor malefunction"
  where triv = obviousFactor f
 
doBerlekamp :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
doBerlekamp f = berlekampFactor' f m
  where m = berlekampBasis f
        berlekampFactor' :: (Show a, Num a, Fractional a, FiniteField a)
                                      => Polynom a -> Matrix a -> [(Int,Polynom a)]
        berlekampFactor' f m | uDegP f <= 1       = [(1,f)]
                             | getNumRowsM m == 1 = [(1,f)]
                             | otherwise         = --trace ("berlekamp f="++show f++" m=\n"++show m)
                              berlekampFactor' g n ++ berlekampFactor' g' n'
          where g  = --trace ("list="++show [x | x <- [ggTP f (h - P [s]) | s <- elems (getReprP f)]
                     --        , x /= 1])$
                     head [x | x <- [ggTP f (h - pKonst s) | s <- elems (getReprP f)]
                             , x /= 1]
                g' = --trace ("f= "++show f) $ 
                     f @/ g
                h  = pList $ getRowM m 2
                n  = newKer m g
                n' = newKer m g'
                newKer m g  = fromListsM $ take r m'
                  where (k,l) = boundsM m
                        m'    = toListsM $ echelonM $ fromListsM
                              [takeFill 0 l $ p2List $ modByP (pList (getRowM m i)) g
                                   | i <- [1..k]]
                        r     = k-1- fromMaybe (-1) (findIndex (all (==0))
                                                        $ reverse m')

takeFill :: Num a => a -> Int -> [a] -> [a]
takeFill a n [] = take n $ cycle [a]
takeFill a n (x:xs) = x : takeFill a (n-1) xs

