{-# LANGUAGE BangPatterns #-}
module Main
  where
import Control.Arrow as A
import GalFld.GalFld
import GalFld.More.SpecialPolys

import System.Random
import System.TimeIt
import Data.List
import Debug.Trace
import Data.Maybe

import GalFld.Core.Polynomials.FFTTuple
{-import GalFld.Core.Polynomials.Conway-}


-- |generiert n zufällige Polynome von Grad d über e
getRndPol :: (Num a, FiniteField a, RandomGen g) => 
                                            g -> Int -> a -> Int -> [Polynom a]
getRndPol gen d e n = map pList $ map findFstNonZero $ splitList (d+1) $ 
        map (els!!) $ take (n*(d+1)) $ randomRs (0,(length els)-1) gen
  where els = elems e

splitList _ [] = []
splitList n xs = (take n xs) : (splitList n $ drop n xs)


findFstNonZero [] = []
findFstNonZero xs 
  | x == 0     = x : (findFstNonZero $ init xs)
  | otherwise = xs
  where x = last xs

evalF f x y = do 
  let res = f x y
  return $! res

main :: IO ()
main = do
  gen <- getStdGen
  let list =  getRndPol gen 1000 (1::F2) 3
  (t,_) <- timeItT (evalF (*) (head list) (head list))
  print t


