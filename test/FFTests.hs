module Main
  where
import GalFld.Core
import GalFld.Algorithmen.Rabin
{-import GalFld.Core.Polynomials.Conway-}

import TestsCommon
import GalFld.Sandbox.FFSandbox

--------------------------------------------------------------------------------
testForExceptions a aMipo = do
  it "x/0 throws exception" $ do
    evaluate (one / FFElem nullP aMipo) `shouldThrow` anyException
    evaluate (a / FFElem nullP aMipo) `shouldThrow` anyException
  it "0/0 throws exception" $
    evaluate (FFElem nullP aMipo / FFElem nullP aMipo) `shouldThrow` anyException

furtherTests e = furtherTests' (elems e) (units e) e
furtherTests' es us e = do
  it "test elemsCount" $
    elemCount e `shouldBe` length es
  it "+ is bijektiv" $
    pMapM_ (\ x -> allUnique [x + y | y <- es] `shouldBe` True) es
  it "* is bijektiv" $
    pMapM_ (\ x -> allUnique [x * y | y <- us] `shouldBe` True) us
  {-it "test recip (full)" $-}
    {-pMapM_ (\ x -> recip x `shouldBe` head [y | y <- us, x * y == one]) us-}
      where allUnique xs = not $
              or [allUnique' (reverse $ take i xs) | i <- [2..(length xs - 1)]]
                where allUnique' (x:xs) = or [x == y | y <- xs]

--------------------------------------------------------------------------------
testSize = 10

tst = hspec $
  describe "tst" $
    it "tst run" $
      pMapM_ (\f -> charRootP (f ^ 2) `shouldBe` f)
        (getAllMonicPs (elems e2e2f2) [1])

main :: IO ()
main = do
  list1 <- rndSelect (getAllPs (elems e2e2f2) [5,4]) testSize
  list2 <- rndSelect (getAllPs (elems e4f2) [5,4]) testSize
  list3 <- rndSelect (getAllPs (elems e3f3) [4,3]) testSize
  hspec $ do
--------------------------------------------------------------------------------
--  in char 2
    describe "GalFld.Core.FiniteFields @e2f2: E2 over F2" $ do
      testFieldSpec e2f2
      furtherTests e2f2
      testForExceptions e2f2 e2f2Mipo
      it "charRootP should be inverse to ^2 (full, up to deg)" $
        pMapM_ (\f -> charRootP (f ^ 2) `shouldBe` f)
        (getAllPs (elems e2f2) [4])
    describe "GalFld.Core.FiniteFields @e4f2: E4 over F2" $ do
      testFieldSpec e4f2
      furtherTests e4f2
      {-it "charRootP should be inverse to ^2 (subset)" $-}
        {-pMapM_ (\f -> charRootP (f ^ 2) `shouldBe` f) list2-}
    describe "GalFld.Core.FiniteFields @e2e2f2: E2 over E2 over F2" $ do
      testFieldSpec e2e2f2
      furtherTests e2e2f2
      it "charRootP should be inverse to ^2 (subset)" $
        pMapM_ (\f -> charRootP (f ^ 2) `shouldBe` f) list1

--------------------------------------------------------------------------------
--  in char 3
    describe "GalFld.Core.FiniteFields @e2f3: E2 over F3" $ do
      testFieldSpec e2f3
      furtherTests e2f3
      it "charRootP should be inverse to ^3 (full)" $
        pMapM_ (\f -> charRootP (f ^ 3) `shouldBe` f)
        (getAllPs (elems e2f3) [4])
    describe "GalFld.Core.FiniteFields @e3f3: E3 over F3" $ do
      testFieldSpec e3f3
      it "charRootP should be inverse to ^3 (subset)" $
        pMapM_ (\f -> charRootP (f ^ 3) `shouldBe` f) list3
    {-
     - Too large:
  list3 <- rndSelect (getAllPs (elems e3e3f3) [4]) testSize
  hspec $ do
    describe "GalFld.Core.FiniteFields @e3e3f3: E3 over E3 over F3" $ do
      testFieldSpec e3e3f3
      it "charRootP should be inverse to ^3 (full, up to deg)" $
        pMapM_ (\f -> charRootP (f ^ 3) `shouldBe` f) list3
     -}
  {-
   - Too large:
  describe "GalFld.Core.FiniteFields @e3e3f3: E3 over E3 over E3 over F3" $
    testFieldSpec e3e3e3f3
   -}
