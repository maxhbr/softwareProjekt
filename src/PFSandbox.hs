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

--------------------------------------------------------------------------------
main :: IO ()
main = hspec $
    describe "Projekt.Core.PrimeFields" $ do
      it "modulus should give the modulus" $ do
        modulus (2 ::Z2) `shouldBe` 2
        modulus (2 ::Z3) `shouldBe` 3
        modulus (2 ::Z5) `shouldBe` 5
        modulus (2 ::Z101) `shouldBe` 101
      it "test Eq" $ do
        (2 ::Z2) `shouldBe` (0 ::Z2)
        (2 ::Z2) `shouldBe` (4 ::Z2)
      it "test for neutral element in Z101" $ mapM_
        (\ x -> x * (1::Z101) `shouldBe` x)
        (units undefined ::[Z101])
      it "x/0 throws exception" $
        evaluate ((3::Z101) / (0::Z101)) `shouldThrow` anyException
      it "0/0 throws exception" $
        evaluate ((0::Z101) / (0::Z101)) `shouldThrow` anyException
      it "1^{-1} == 1" $
        recip (1::Z101) `shouldBe` (1 ::Z101)
      it "test invMod (x/x=1) in Z101" $ mapM_
        (\ x -> x / x `shouldBe` (1 ::Z101))
        (units undefined ::[Z101])
      it "test invMod (x/x*x=x) in Z101" $ mapM_
        (\ x -> x / x * x `shouldBe` x)
        (units undefined ::[Z101])
