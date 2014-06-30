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
import Control.DeepSeq

import Data.List
import Debug.Trace
import Data.Maybe

import GalFld.Core.Polynomials.FFTTuple
{-import GalFld.Core.Polynomials.Conway-}


-- |generiert n zufällige Polynome von maximal Grad d über e
getRndPol :: (Num a, FiniteField a, RandomGen g) =>
                                            g -> Int -> a -> Int -> [Polynom a]
getRndPol gen d e n = map (pList . findFstNonZero) (splitList (d + 1)
  $ map (els !!)
  $ take (n * (d + 1))
  $ randomRs (0, length els - 1) gen)
    where els = elems e

splitList _ [] = []
splitList n xs = take n xs : splitList n (drop n xs)


findFstNonZero [] = []
findFstNonZero xs
  | x == 0     = x : findFstNonZero (init xs)
  | otherwise = xs
  where x = last xs

--------------------------------------------------------------------------------

samples = 30::Int

benchMult gen e = [ benchMult' gen p
  | p <- map ((\ n -> (n, getRndPol gen n e samples)) . (2 ^)) [1..7] ]

benchMult' gen (n,list) = bgroup ("multBench @ "++show n)
  [ bench ("multNorm @ "++show n) $ nf (multBench (*)) list,
    bench ("multKar @ "++show n) $ nf (multBench multPK) list,
    bench ("multFFT @ "++show n) $ nf (multBench ssP) list ]

multBench mulFunc list = zipWith mulFunc (take n list) (drop n list)
  where n = length list `quot` 2

-------------------------------------------------------------------------------
myConfig = defaultConfig {
    cfgSamples = M.Last (Just (10::Int))}

main :: IO ()
main = do
  gen <- getStdGen
  defaultMainWith myConfig (return ()) $
    benchMult gen (FFElem (pList [0,1::F5]) (pList [3,3,0,1]))
