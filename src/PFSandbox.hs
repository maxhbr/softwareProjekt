--------------------------------------------------------------------------------
-- |
-- Module      : PFSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von PrimeFields gedacht.
--
--------------------------------------------------------------------------------

module PFSandbox where
import Projekt.Core

import Test.Hspec
import Control.Exception (evaluate)

import SpecCommon

--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
    describe "Projekt.Core.PrimeFields" $ do
      it "modulus should give the modulus" $ do
        modulus (2 ::F2) `shouldBe` 2
        modulus (2 ::F3) `shouldBe` 3
        modulus (2 ::F5) `shouldBe` 5
        modulus (2 ::F101) `shouldBe` 101
      it "test Eq" $ do
        (2 ::F2) `shouldBe` (0 ::F2)
        (2 ::F2) `shouldBe` (4 ::F2)
      it "1^{-1} == 1" $
        recip (1::F101) `shouldBe` (1 ::F101)
      it "x/0 throws exception" $
        evaluate ((3::F101) / (0::F101)) `shouldThrow` anyException
      it "0/0 throws exception" $
        evaluate ((0::F101) / (0::F101)) `shouldThrow` anyException
      {-
      it "test invMod (x/x=1) in F101" $ mapM_
        (\ x -> x / x `shouldBe` (1 ::F101))
        (units undefined ::[F101])
      it "test invMod (x/x*x=x) in F101" $ mapM_
        (\ x -> x / x * x `shouldBe` x)
        (units undefined ::[F101])
       -}
    describe "Projekt.Core.PrimeFields @F2" $
      testFieldSpec (1::F2)
    describe "Projekt.Core.PrimeFields @F3" $
      testFieldSpec (1::F3)
    describe "Projekt.Core.PrimeFields @F5" $
      testFieldSpec (1::F5)
    describe "Projekt.Core.PrimeFields @F101" $
      testFieldSpec (1::F101)
