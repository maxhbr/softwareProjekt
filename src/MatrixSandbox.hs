--------------------------------------------------------------------------------
-- |
-- Module      : MatrixSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--
--
--------------------------------------------------------------------------------

module MatrixSandbox where
import Projekt.Core hiding (examplePoly, examplePoly')
import Projekt.Core.Matrix

import Test.Hspec
import Control.Exception (evaluate)

import SpecCommon

--------------------------------------------------------------------------------
--  Matrizen

m = M [[7::F5, 8::F5, 9::F5], [4::F5, 5::F5, 6::F5], [1::F5, 2::F5, 3::F5]]
m23 = M [[7::F5, 8::F5, 9::F5], [4::F5, 5::F5, 6::F5]]
m32 = M [[7::F5, 8::F5], [4::F5, 5::F5], [1::F5, 2::F5]]

eye2 = M [[1::F5, 0], [0, 1::F5]]
eye3 = M [[1::F5, 0, 0], [0, 1::F5, 0], [0, 0, 1::F5]]
eye = Mdiag (1::F5)

--------------------------------------------------------------------------------
--  Matrizen von Polynomen

mp = M [[P[0,1::F5],P[0,0,1::F5]],[P[1::F5],P[0,1::F5]]]

--------------------------------------------------------------------------------
-- TODO: Test Matrix Multiplikation
main :: IO ()
main = hspec $ do
    describe "Projekt.Core.Matrix" $ do
      it "det(eye) = 1 (1,2,3,alg)" $ do
        detM (M[[1::F7]]) `shouldBe` 1
        detM eye2 `shouldBe` 1
        detM eye3 `shouldBe` 1
        detM eye `shouldBe` 1
      it "det(2 *eye) = 2^n" $ do
        detM (M[[2::F7]]) `shouldBe` 2
        detM (eye2 * Mdiag 2) `shouldBe` 4
        detM (eye3 * Mdiag 2) `shouldBe` 8
