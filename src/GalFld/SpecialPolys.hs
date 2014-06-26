module GalFld.SpecialPolys 
  where

import Debug.Trace

import GalFld.NumberTheory
import GalFld.Core
import GalFld.GalFld

import qualified Control.Arrow as A
import Data.List


-- |Die Kreisteilungspolynome
--  gibt das n-te Kreisteilungspolynom über dem Körper dem e zu Grunde liegt
cyclotomicPoly :: (Show a, Fractional a, Num a, FiniteField a) => 
                                                          Int -> a -> Polynom a
cyclotomicPoly 1 e  = pTupUnsave [(1,1),(0,-1)]
cyclotomicPoly n e  
  | isPrime n = pTupUnsave $ map (\i -> (i,1)) $ reverse [0..n-1]
  | otherwise = foldl (@/) numerator $ map fst $ filter (\(_,m) -> m==(-1)) l
  where numerator = product $ map fst $ filter (\(_,m)->m==1) l
        l = [(pTupUnsave [(n `quot` d, 1), (0,-1)], möbFkt d) | d <- divisors n]


-- | Gibt das Pi-Polynom zu f
--   f muss ein monischer Teiler von x^m -1 über F_q sein
piPoly :: (Show a, Num a, Fractional a, FiniteField a) =>
                                                        Polynom a -> Polynom a
piPoly f
  | isSqfree  = piSqFree
  | otherwise = piSqFree `odot` fNonSqFree
  where -- P_(tau f), wobei tau f der quadratfreie Teil von f ist
        piSqFree = foldl (\p f -> (p `odot` f) @/ p) pFst (map snd $ tail facs)
        -- Faktorisierung von f
        facs = factorP f
        -- Start der Rekursion mit P_(f1)
        pFst = (assozPoly $ snd $ head facs) @/ (pTupUnsave [(1,1)])
        -- Definition von odot
        odot f g = evalPInP f $ assozPoly g
        -- Test auf quadratfrei
        isSqfree = all (==1) $ map fst facs
        -- f / tau(f)
        fNonSqFree = unFact $ map (A.first (\i -> i-1)) facs



{-# INLINE assozPoly #-}
assozPoly :: (Num a, FiniteField a) => Polynom a -> Polynom a
assozPoly f = pTupUnsave $ map (A.first (q^)) $ p2Tup f
  where q = elemCount $ getReprP f


{-# INLINE evalPInP #-}
-- |evalPinP f g = f(g(x))
evalPInP :: (Eq a, Num a) => Polynom a -> Polynom a -> Polynom a
evalPInP f g = evalP' g $ p2Tup f
evalP' g []   = 0
evalP' g fs   = snd $ foldl' (\(i,h) (j,y) -> (j,h*g^(i-j)+y)) 
                                                         (head fs') (tail fs') 
  where fs' = map (A.second (pKonst)) fs
