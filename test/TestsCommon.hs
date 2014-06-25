module TestsCommon
  ( module X
  , pMapM_
  , rndSelect
  , testFieldSpec
  ) where
import GalFld.Core.FiniteField

-- from hspec
import Test.Hspec as X
import Control.Exception as X

-- from monad-parallel
import qualified Control.Monad.Parallel as P

import System.Random
import Control.Monad (replicateM)

rndSelect xs n = do
    gen <- getStdGen
    return $ take n [xs!!x | x <- randomRs (0, length xs - 1) gen]

pMapM_  f = P.sequence_ . map f

testFieldSpec e = testFieldSpec' $ elems e
testFieldSpec' es = do
  it "Assoziativität"
    (testAsso es `shouldBe` True)
  it "Kommutativität"
    (testKommu es `shouldBe` True)
  it "Einheiten"
    (testEinh es `shouldBe` True)
  it "Inversen"
    (testInv es `shouldBe` True)
  it "Distributivität"
    (testDist es `shouldBe` True)
