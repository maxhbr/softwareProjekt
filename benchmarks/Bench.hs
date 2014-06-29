{-# LANGUAGE CPP #-}
module Main
  where
import Criterion.Main
import System.Random

import GalFld.GalFld
import GalFld.Algorithmen
import GalFld.Sandbox.FFSandbox (f2,e2f2,e2e2f2,e4f2,e4f2Mipo)
import GalFld.More.SpecialPolys

-- #define BENCHFINDIRREDS
#ifdef BENCHFINDIRREDS
benchFindIrreds =
  [ bgroup "findIrreds e2f2"
    [ bench "3" $ whnf findIrreds (getAllMonicPs es1 [3])
    , bench "5" $ whnf findIrreds (getAllMonicPs es1 [5])
    , bench "7" $ whnf findIrreds (getAllMonicPs es1 [7])
    , bench "9" $ whnf findIrreds (getAllMonicPs es1 [9]) ]
  , bgroup "findIrreds e2e2f2"
    [ bench "3" $ whnf findIrreds (getAllMonicPs es2 [3])
    , bench "5" $ whnf findIrreds (getAllMonicPs es2 [5])
    , bench "7" $ whnf findIrreds (getAllMonicPs es2 [7])
    , bench "9" $ whnf findIrreds (getAllMonicPs es2 [9]) ] ]
  where es1 = elems e2f2
        es2 = elems e2e2f2
#endif

-- #define HEAVYBENCH
#ifdef HEAVYBENCH
heavyBench :: Int -> [(Int,Int)]
heavyBench n = foldl multPM f (replicate (n-1) f)
  where f = [(11::Int,1::Int),(8,2),(6,1),(5,1),(3,2),(2,2),(0,1)]

benchHeavyBench = 
  [ bgroup "heavyBench"
    [ bench "100" $ whnf heavyBench 100
    , bench "250" $ whnf heavyBench 250
    , bench "500" $ whnf heavyBench 500 ] ]
#endif

-- #define BENCH_PRIMNORM
#ifdef BENCH_PRIMNORM
primNormBench n = factorP $ 
  ggTP (piPoly $ pTupUnsave [(n,1::F2),(0,-1)]) 
       (cyclotomicPoly (2^n-1) (1::F2)) 

benchPrimNorm = [bgroup "find primitive normal polys"
  [bench "8" $ whnf primNormBench 8] ]
#endif

-- Testwise
#define BENCH_DIV
benchDivInv f g = divPInv f g
benchDivHorn f g = divP f g

benchDiv l = [bgroup "divP: Horner <-> InvMod"
  [bgroup ("degs "++show (n,m))
    [bench "horn" $ whnf (benchDivHorn f) g ,
     bench "inv" $ whnf (benchDivInv f) g] ]
    | f <- take 100 l,
      g <- drop 100 l, 
      let n = uDegP f, let m = uDegP g]
--
rndSelect xs n = do
    gen <- getStdGen
    return $ take n [x | x <- randomRs (0, length xs - 1) gen]


main = do
  l1 <- rndSelect (getAllMonicPs (elems (1::F5)) [1..8]) 200
  defaultMain $
#ifdef BENCHFINDIRREDS
    benchFindIrreds ++
#endif
#ifdef HEAVYBENCH
    benchHeavyBench ++
#endif
#ifdef BENCH_PRIMNORM
    benchPrimNorm ++
#endif
#ifdef BENCH_DIV
    benchDiv l1 ++
#endif
    []
