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

m = fromListsM [ [7::F5, 8::F5, 9::F5]
               , [4::F5, 5::F5, 6::F5]
               , [1::F5, 2::F5, 3::F5] ]
mNosSing = fromListsM [ [7::F5, 8::F5, 9::F5]
                      , [4::F5, 5::F5, 6::F5]
                      , [0::F5, 2::F5, 3::F5] ]
m23 = fromListsM [ [7::F5, 8::F5, 9::F5]
                 , [4::F5, 5::F5, 6::F5] ]
m32 = fromListsM [ [7::F5, 8::F5]
                 , [4::F5, 5::F5]
                 , [1::F5, 2::F5] ]

eye2 = fromListsM [ [1::F5, 0]
                  , [0, 1::F5] ]
eye3 = fromListsM [ [1::F5, 0, 0]
                  , [0, 1::F5, 0]
                  , [0, 0, 1::F5] ]
eye = Mdiag (1::F5)

m'  = fromListsM [ [0::F5, 1, 0,  1, 0 ]
                   , [0, -2, 0,  0, 0 ]
                   , [0,  0, 0,  0, 0 ]
                   , [0,  0, 0, -2, 0 ]
                   , [0,  1, 0,  1, 0 ]]

m''  = fromListsM [ [0::F5, 0, 1 ]
                   , [0, 1, 0 ]
                   , [0, 1, 0 ] ]

--------------------------------------------------------------------------------
--  Matrizen von Polynomen

mp = fromListsM [ [ P[0,1::F5], P[0,0,1::F5] ]
                , [ P[1::F5], P[0,1::F5] ] ]
mp2 = fromListsM [ [ P[0,1::F5], P[0,0,1::F5] ]
                 , [ P[0,1::F5], P[0,1::F5] ] ]

--------------------------------------------------------------------------------
-- TODO: Test Matrix Multiplikation
main :: IO ()
main = hspec $ do
    describe "Projekt.Core.Matrix" $ do
      it "eye is multiplikative neutral" $ do
        m * eye3 `shouldBe` m
        m23 * eye2 `shouldBe` m23
        m32 * eye3 `shouldBe` m32
        m * eye `shouldBe` m
        m23 * eye `shouldBe` m23
        m32 * eye `shouldBe` m32
        eye * m `shouldBe` m
        eye * m23 `shouldBe` m23
        eye * m32 `shouldBe` m32
      it "det(eye) = 1 (1,2,3,alg)" $ do
        detLapM (fromListsM[[1::F7]]) `shouldBe` 1
        detLapM eye2 `shouldBe` 1
        detLapM eye3 `shouldBe` 1
        detLapM eye `shouldBe` 1
      it "det(2 * eye) = 2^n" $ do
        detLapM (fromListsM[[2::F7]]) `shouldBe` 2
        detLapM (eye2 * Mdiag 2) `shouldBe` 4
        detLapM (eye3 * Mdiag 2) `shouldBe` 8
