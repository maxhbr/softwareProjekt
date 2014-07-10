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


{-instance (Numeral a, NFData a) => NFData (Mod a) where-}
  {-rnf _ = () -- rnf . unMod-}

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

benchBerle gen e desc = [ benchBerle' gen p
  | p <- map ((\ n -> (n, getRndPol gen n e samples)) . (2 ^)) [1..5] ]
  where benchBerle' gen (n,list) = bgroup ("berleBench "++desc++" @ "++show n)
          [ bench ("Berlekamp @ "++show n) $ nf (berleBench factorP) list,
            bench ("Berlekamp2 @ "++show n) $ nf (berleBench factorP2) list]

berleBench berleFunc list = map berleFunc list

-------------------------------------------------------------------------------
myConfig = defaultConfig {
    cfgSamples = M.Last (Just (10::Int))}

main :: IO ()
main = do
  gen <- getStdGen
  defaultMainWith myConfig (return ()) $
    benchBerle gen (1::F5) "F5"
    {-benchBerle gen (FFElem (pList [0,1::F5]) (pList [3,3,0,1])) "F5^3"-}
    {-benchDiv gen (FFElem (pList [0,1::F5]) (pList [2,1,4,2,3,3,0,0,0,0,1])) "F5^10"-}
