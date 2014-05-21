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
  -- Algorithmus
  , berlekampBasis
  , berlekampFactor
  , newKer
  )where
import Data.Maybe
import Data.List
import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials
import Projekt.Core.Matrix
import Projekt.Core.Factorization
import Debug.Trace
import Projekt.Algorithmen.SFreeFactorization

--------------------------------------------------------------------------------
--  Wrapper

berlekamp :: (Show a ,FiniteField a, Num a, Fractional a) => Polynom a -> [(Int,Polynom a)]
berlekamp = appFact berlekampFactor . obviousFactor

appBerlekamp :: (Show a, FiniteField a, Num a, Fractional a) => [(Int,Polynom a)] -> [(Int,Polynom a)]
appBerlekamp = appFact berlekamp

-- |Faktorisiert ein Polynom f über einem endlichen Körper
-- Benutzt wird dazu die Quadratfreie Faktorisierung mit anschließendem
-- Berlekamp
sffAndBerlekamp :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
sffAndBerlekamp f = appBerlekamp $ sff f

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
                             | otherwise         = berlekampFactor' g n
                                                 ++ berlekampFactor' g' n'
          where g  = head [x | x <- [ggTP f (h - P [s]) | s <- elems (getReprP f)]
                             , x /= 1]
                g' = f @/ g
                h  = P $ getRowM m 2
                n  = newKer m g
                n' = newKer m g'
                newKer m g  = fromListsM $ take r m'
                  where (k,l) = boundsM m
                        m'    = toListsM $ echelonM $ fromListsM $ take r'' m''
                        m''   = [take l $ unP $ modByP (P (getRowM m i)) g
                                                                | i <- [1..k]]
                        r     = k-1- fromMaybe (-1) (findIndex (all (==0))
                                                        $ reverse m')
                        r''   = k-1- fromMaybe (-1) (findIndex (all (==0))
                                                        $ reverse m'')
