module Main
  where
import TestsCommon

import GalFld.Core hiding (examplePoly, examplePoly')
import GalFld.Core.Matrix

import GalFld.Sandbox.FFSandbox
import GalFld.Sandbox.MatrixSandbox

--------------------------------------------------------------------------------
-- TODO: Test Matrix Multiplikation
testSize = 10
main :: IO ()
main = do
  hspec $
    describe "GalFld.Core.Matrix" $ do
      it "eye is multiplikative neutral" $ do
        m * eye3 `shouldBe` m
        m23 * eye3 `shouldBe` m23
        m32 * eye2 `shouldBe` m32
        m * eye `shouldBe` m
        m23 * eye `shouldBe` m23
        m32 * eye `shouldBe` m32
        eye * m `shouldBe` m
        eye * m23 `shouldBe` m23
        eye * m32 `shouldBe` m32
      it "detLapM(eye) = 1 (1,2,3,alg)" $ do
        detLapM (fromListsM[[1::F7]]) `shouldBe` 1
        detLapM eye2 `shouldBe` 1
        detLapM eye3 `shouldBe` 1
        detLapM eye `shouldBe` 1
      it "detLapM(2 * eye) = 2^n" $ do
        detLapM (fromListsM[[2::F7]]) `shouldBe` 2
        detLapM (eye2 * Mdiag 2) `shouldBe` 4
        detLapM (eye3 * Mdiag 2) `shouldBe` 8
      it "detLapM == detM (F3 (2,2))" $
        pMapM_ (\ x ->  detLapM x `shouldBe` detM x )
          (getAllM (elems (1::F3)) (2,2))
      it "detLapM == detM (e2f2 (2,2))" $
        pMapM_ (\ x ->  detLapM x `shouldBe` detM x )
          (getAllM (elems e2f2) (2,2))
      {-
      it "detLapM == detM (e2e2f2 (2,2))" $
        pMapM_ (\ x ->  detLapM x `shouldBe` detM x )
          (getAllM (elems e2e2f2) (2,2))
       -}
      {-
  list <- rndSelect (getAllM (elems e2e2f2) (3,3)) testSize
  hspec $
    describe "GalFld.Core.Matrix" $ do
      it "detLapM == detM (e2e2f2 (3,3) subset)" $
        pMapM_ (\ x ->  detLapM x `shouldBe` detM x ) list
       -}
