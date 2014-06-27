{-# LANGUAGE CPP, BangPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Algorithmen.Berlekamp
-- Note        : Implementiert eine Berlekamp Faktorisierung
--
-- Funktioniert nur auf Quadratfreien Polynomen
--
-- Enthält den 1967 von Elwyn Berlekamp enwickelten Berlekamp-Algorithmus zur
-- Faktorisierung von Polynomen über endlichen Körpern.
--
--------------------------------------------------------------------------------
module GalFld.Algorithmen.Berlekamp
  ( berlekamp, appBerlekamp, sffAndBerlekamp
  , findIrred, findIrreds, findTrivialsB
  -- Algorithmus
  , berlekampBasis
  , berlekampFactor
  )where

import Debug.Trace

import Data.Maybe
import Data.List
import Control.Monad
import Control.Parallel
import Control.Parallel.Strategies

import GalFld.Core
import GalFld.Algorithmen.SFreeFactorization

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
#if 0
-- Ist lazy.
findIrreds :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [Polynom a]
findIrreds (f:fs) = findIrreds' (f:fs)
  where findIrreds' []     = []
        findIrreds' (f:fs)
          | (not (hasNs f es) || uDegP f < 2)
              && isTrivialFact fSff
              && isTrivialFact fB              = f : findIrreds' fs
          | otherwise                          = findIrreds' fs
          where fSff = appSff $ toFact f
                fB   = appBerlekamp fSff
        es = elems $ getReprP f
#else
-- mit backtracking
findIrreds fs = do
  f <- fs
  let fSff = appSff $ toFact f
  guard (isTrivialFact fSff)
  let fB = appBerlekamp fSff
  guard (isTrivialFact fB)
  return f
#endif

-- |Gibt alle Faktorisierungen zurück, welche nach Berlekamp noch trivial sind
-- Wendet zuvor (die offensichtliche Faktorisierung und) SFF an
--
-- Ist parallelisiert mittels Strategie rpar.
findTrivialsB :: (Show a, Fractional a, Num a, FiniteField a) => [Polynom a] -> [[(Int,Polynom a)]]
findTrivialsB ps = [fs | fs <- parMap rpar appBerlekamp (findTrivialsSff ps)
                     , isTrivialFact fs]

--------------------------------------------------------------------------------
--  Algorithmus

-- |Berechnet eine Basis des Berlekampraums zu f,
--  d.h. gibt eine Matrix zurück, deren Zeilen gerade den Berlekampraum
--  aufspannen bzgl der kanonischen Basis { 1, x, x², x³, ... }
berlekampBasis :: (Show a, Fractional a, Num a, FiniteField a)
                                                       => Polynom a -> Matrix a
berlekampBasis f = transposeM $ kernelM $ transposeM $!
                        fromListsM [red i | i <- [0..(n-1)]] - genDiagM 1 n
  where !n     = fromJust $ degP f
        !q     = elemCount a
        !a     = getReprP f
        {-# INLINE red #-}
        red i = takeFill 0 n $ p2List $ modByP (pTupUnsave [(i*q,1)]) f

-- |Faktorisiert ein Polynom f über einem endlichen Körper
--  Voraussetzungen: f ist quadratfrei
--  Ausgabe: Liste von irreduziblen, pw teilerfremden Polynomen
berlekampFactor :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
#if 0
berlekampFactor f
    | length triv == 1 = berleFac
    | length triv == 2 = head triv : berleFac
    | otherwise       = error "obviousFactor malefunction"
  where triv     = obviousFactor f
        berleFac = doBerlekamp $ snd $ last triv

doBerlekamp :: (Show a, Fractional a, Num a, FiniteField a)
                                              => Polynom a -> [(Int,Polynom a)]
doBerlekamp f = berlekampFactor' f m
  where m = berlekampBasis f
        berlekampFactor' :: (Show a, Num a, Fractional a, FiniteField a)
                                      => Polynom a -> Matrix a -> [(Int,Polynom a)]
        berlekampFactor' f m | uDegP f <= 1       = --trace ("berlekamp f="++show f++" deg f <= 1") $
                                                    [(1,f)]
                             | getNumRowsM m == 1 = --trace ("berlekamp f="++show f++" triv m="++show m) $
                                                    [(1,f)]
                             | otherwise         = --trace ("berlekamp f="++show f++" m=\n"++show m)
                              berlekampFactor' g n ++ berlekampFactor' g' n'
          where g  = --trace ("list="++show [x | x <- [ggTP f (h - pKonst s) | s <- elems (getReprP f)]
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
#else
berlekampFactor f | isNullP f   = []
                  | uDegP f < 2 = [(1,f)]
                  | otherwise   = berlekampFactor' f m
  where !m = berlekampBasis f
        {-# INLINE berlekampFactor' #-}
        berlekampFactor' :: (Show a, Num a, Fractional a, FiniteField a)
                                      => Polynom a -> Matrix a -> [(Int,Polynom a)]
        berlekampFactor' f m | uDegP f <= 1       = [(1,f)]
                             | getNumRowsM m == 1 = [(1,f)]
                             | otherwise         = --trace ("berlekamp f="++show f++" m=\n"++show m)
                              berlekampFactor' g n ++ berlekampFactor' g' n'
          where {-# INLINE g #-}
                g  = --trace ("list="++show [x | x <- [ggTP f (h - pKonst s) | s <- elems (getReprP f)]
                     --        , x /= 1])$
                     head [x | x <- [ggTP f (h - pKonst s) | s <- elems (getReprP f)]
                             , x /= 1]
                {-# INLINE g' #-}
                g' = --trace ("f= "++show f) $ 
                     f @/ g
                {-# INLINE h #-}
                h  = pList $ getRowM m 2
                {-# INLINE n #-}
                n  = newKer m g
                {-# INLINE n' #-}
                n' = newKer m g'
                {-# INLINE newKer #-}
                newKer m g  = fromListsM $! take r m'
                  where !(k,l) = boundsM m
                        !m'    = toListsM $ echelonM $ fromListsM
                              [takeFill 0 l $ p2List $ modByP (pList (getRowM m i)) g
                                   | i <- [1..k]]
                        !r     = k-1- fromMaybe (-1) (findIndex (all (==0))
                                                        $ reverse m')

#endif

{-# INLINE takeFill #-}
takeFill :: Num a => a -> Int -> [a] -> [a]
takeFill a n [] = replicate n a
takeFill a n (x:xs) = x : takeFill a (n-1) xs

