module Main
  where
import Criterion.Main

import GalFld.Core
import GalFld.Algorithmen
import GalFld.Sandbox.FFSandbox (f2,e2f2,e2e2f2,e4f2,e4f2Mipo)

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

main = defaultMain benchFindIrreds
