{-# LANGUAGE BangPatterns #-}
module Main
  where
import Control.Arrow as A
import GalFld.GalFld
import GalFld.More.SpecialPolys

import System.Random
import Criterion.Main
import Criterion.Config
import qualified Data.Monoid as M

import Data.List
import Debug.Trace
import Data.Maybe

import GalFld.Core.Polynomials.FFTTuple
{-import GalFld.Core.Polynomials.Conway-}


-- |generiert n zufällige Polynome von maximal Grad d über e
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

--------------------------------------------------------------------------------

samples = 30::Int

benchMul gen e = [ bgroup "multBench" $ 
  concat [ [bench ("multNorm @ "++show n) $ whnf (mulBench (*)) list,
    bench ("multKar @ "++show n) $ whnf (mulBench (multPK)) list,
    bench ("multFFT @ "++show n) $ whnf (mulBench (ssP)) list]
    | (n,list) <- map (\n -> (n,getRndPol gen n e samples)) $ map (2^) [1..10] ] ]

mulBench mulFunc list = zipWith (mulFunc) (take n list) (drop n list)
  where n = (length list) `quot` 2

-------------------------------------------------------------------------------
myConfig = defaultConfig {
    cfgSamples = M.Last (Just (10::Int))}

main :: IO ()
main = do
  gen <- getStdGen
  defaultMainWith myConfig (return ()) $
    benchMul gen (1::F2)


