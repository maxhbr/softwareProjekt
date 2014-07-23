module Main
  where

import Data.List

import TestsCommon

import GalFld.Core
import GalFld.Algorithmen

import GalFld.Sandbox.FFSandbox (f2,e2f2,e2e2f2,e4f2,e4f2Mipo)
import GalFld.Sandbox.PolySandbox hiding (testSize, main)
import GalFld.Sandbox.AlgSandbox

import GalFld.More.NumberTheory

--------------------------------------------------------------------------------

-- |Anzahl monischer irreduzibler Polynome von Grad n über F_q
countMonicIrreds :: Int -> Int -> Int
countMonicIrreds q n = (sum [(moebFkt d)*q^(n `quot` d) | d <- divisors n]) `quot` n


-- |Anzahl monischer irreduzibler Polynome von Grad n über F_q
countIrreds :: Int -> Int -> Int
countIrreds q n = (q-1)*(countMonicIrreds q n)


--------------------------------------------------------------------------------
testSize = 100


irredTestsF2 func = [it ("findIrreds von Grad "++show n++" über F_2") $
    (length $ func $ getAllMonicPs (elems (1::F2)) [n])
    `shouldBe` (countMonicIrreds 2 n) | n <- [1..10]]
irredTestsF3 func = [it ("findIrreds von Grad "++show n++" über F_3") $
    (length $ func $ getAllMonicPs (elems (1::F3)) [n])
    `shouldBe` (countMonicIrreds 3 n) | n <- [1..10]]
irredTestsF5 func = [it ("findIrreds von Grad "++show n++" über F_5") $
    (length $ func $ getAllMonicPs (elems (1::F5)) [n])
    `shouldBe` (countMonicIrreds 5 n) | n <- [1..6]]

--------------------------------------------------------------------------------
compareBerlekampRabin = it "compare Berlekamp und Rabin" $
  and ls `shouldBe` True
    where ls = map (\f -> (isTrivialFact (sffAndBerlekamp f))== rabin f)
                   $ take 100
                   $ getAllMonicPs (elems (1 :: F3)) [40]

main :: IO ()
main = do
  list1 <- rndSelect (getAllPs (elems e2e2f2) [5,4]) testSize
  list2 <- rndSelect (getAllPs (elems e4f2) [5,4]) testSize
  hspec $ do
    describe "GalFld.Algorithmen.Rabin" $ do
      compareBerlekampRabin
    describe "GalFld.Algorithmen.SFreeFactorization" $ do
      it "sff and unFact should be inverse (example f over F3)" $
        unFact (sff f) `shouldBe` f
      it "sff and unFact should be inverse (up to deg 10 of f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f)
        (getAllP (elems (0::F2)) 10)
      it "sff and unFact should be inverse (up to deg 7 of e2f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f)
        (getAllP (elems e2f2) 7)
      it "sff and unFact should be inverse (random subset of e2e2f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f) list1
      it "sff and unFact should be inverse (random subset of e4f2)" $
        mapM_ (\f -> unFact (sff f) `shouldBe` f) list2
    describe "GalFld.Algorithmen.Berlekamp" $ do
      it "sffAndBerlekamp and unFact should be inverse (example f over F3)" $
        unFact (sffAndBerlekamp f) `shouldBe` f
      it "sffAndBerlekamp and unFact should be inverse (example bFailF)" $
        unFact (sffAndBerlekamp bFailF) `shouldBe` bFailF
      it "sffAndBerlekamp and unFact should be inverse (example sffFailF)" $
        unFact (sffAndBerlekamp sffFailF) `shouldBe` sffFailF
      it "sffAndBerlekamp and unFact should be inverse (random subset of e2e2f2)" $
        pMapM_ (\f -> unFact (sffAndBerlekamp f) `shouldBe` f) $
        take (quot testSize 50 + 1) list1
      it "sffAndBerlekamp and unFact should be inverse (random subset of e4f2)" $
        mapM_ (\f -> unFact (sffAndBerlekamp f) `shouldBe` f) $
        take (quot testSize 50 + 1) list2
    {-describe "Teste Irreduzibilität" $ do-}
      {-sequence_ (irredTestsF2 findIrreds)-}
      {-sequence_ (irredTestsF3 findIrreds)-}
      {-sequence_ (irredTestsF5 findIrreds)-}
    {-describe "Teste Irreduzibilität (Rabin)" $ do-}
      {-sequence_ (irredTestsF2 findIrredsRabin)-}
      {-sequence_ (irredTestsF3 findIrredsRabin)-}
      {-sequence_ (irredTestsF5 findIrredsRabin)-}


