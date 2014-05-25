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
{-
  where irreds = [unFact fs | fs <- map appBerlekamp
                       [fs | fs <- map appSff
                             [(toFact . aggP) f | f <- ps , f /= P[]]
                           , isTrivialFact fs]
                     , isTrivialFact fs]
 -}

-- |Filtert mittels SFF und Berlekamp aus einer Liste die irreduzibleneiner
-- liste heraus
findIrreds :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [Polynom a]
findIrreds ps = parMap rpar unFact $ findTrivialsB ps

-- |Gibt alle Faktorisierungen zurück, welche nach Berlekamp noch trivial sind
-- Wendet zuvor (die offensichtliche Faktorisierung und) SFF an
findTrivialsB :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [[(Int,Polynom a)]]
findTrivialsB ps = [fs | fs <- map appBerlekamp
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
        a     = prodOfCoeffsP f
        red i = take n (unP (snd (divP (fromMonomialsP [(i*q,1)]) f))
                              ++ [0*a | i <- [0..]] )

-- |Faktorisiert ein Polynom f über einem endlichen Körper
--  Voraussetzungen: f ist quadratfrei
--  Ausgabe: Liste von irreduziblen, pw teilerfremden Polynomen
berlekampFactor :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
berlekampFactor (P[]) = [(1,P[])]
berlekampFactor (P[m]) = [(1,P[m])]
berlekampFactor (P[m0,m1]) = [(1,P[m0,m1])]
berlekampFactor f = berlekampFactor' f m
  where m = berlekampBasis f
        berlekampFactor' :: (Show a, Num a, Fractional a, FiniteField a)
                                      => Polynom a -> Matrix a -> [(Int,Polynom a)]
        berlekampFactor' f m | uDegP f <= 1       = [(1,f)]
                             | getNumRowsM m == 1 = [(1,f)]
                             | otherwise         = --trace ("berlekamp f="++show f++" m=\n"++show m)
                              (berlekampFactor' g n ++ berlekampFactor' g' n')
          where g  = --trace ("list="++show [x | x <- [ggTP f (h - P [s]) | s <- elems (getReprP f)]
                     --        , x /= 1])$
                     head [x | x <- [ggTP f (h - P [s]) | s <- elems (getReprP f)]
                             , x /= 1]
                g' = --trace ("f= "++show f) $ 
                     f @/ g
                h  = P $ getRowM m 2
                n  = newKer m g
                n' = newKer m g'
                newKer m g  = fromListsM $ take r m'
                  where (k,l) = boundsM m
                        m'    = toListsM $ echelonM $ fromListsM
                              [takeFill 0 l $ unP $ modByP (P (getRowM m i)) g
                                   | i <- [1..k]]
                        r     = k-1- fromMaybe (-1) (findIndex (all (==0))
                                                        $ reverse m')


takeFill :: Num a => a -> Int -> [a] -> [a]
takeFill a n [] = take n $ cycle [a]
takeFill a n (x:xs) = x : (takeFill a (n-1) xs)

