{-# LANGUAGE CPP #-}
module Main
  where
import TestsCommon
import GalFld.Core
import GalFld.Sandbox.FFSandbox (e2f2,e2e2f2,e4f2)
import GalFld.Sandbox.PolySandbox

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
subroutineAdd list = do
  it "test * (x+0=x)" $
    pMapM_ (\ x -> x + pKonst 0 `shouldBe` x) list
  it "test * (x-x=0)" $
    pMapM_ (\ x -> x - x `shouldBe` pKonst 0) list

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
subroutineMult list = do
  it "test * (x*1=x)" $
    pMapM_ (\ x -> x * pKonst 1 `shouldBe` x) list
  it "xy=yx" $
    pMapM_ (\ (x,y) -> x * y == y * x `shouldBe` True) $
          zip (take testSize list) (drop testSize list)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
subroutineDiv list = do
  it "test divP (x/x=1)" $
    pMapM_ (\ x -> divPInv x x `shouldBe` (pKonst 1, nullP)) list
  it "test divP generally" $
    pMapM_ (\ (x,y) -> unDivP (divPInv x y) x y `shouldBe` True) $
          zip (take testSize list) (drop testSize list)
#if 0
  it "test divPHorner <-> divPInv" $
    pMapM_ (\ (x,y) -> divPInv x y `shouldBe` divP x y) $
          zip (take testSize list) (drop testSize list)
#endif
  it "x/0 throws exception" $
    pMapM_ (\x -> evaluate (divP x nullP) `shouldThrow` anyException) list
  it "test eekP" $
    pMapM_ (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True) $
          zip (take testSize list) (drop testSize list)
  where unDivP (q,r) a b = a == q * b + r
        unEekP (d,s,t) a b = d == s*a + t*b

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
subroutine list = do
  subroutineAdd list
  subroutineMult list
  subroutineDiv list

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
testSize = 3
main :: IO ()
main = do
  list  <- rndSelect (getAllP (elems undefined ::[F5]) 10) (2*testSize)
  vList <- rndSelect (getAllP (elems e2f2) 10)            (2*testSize)
  wList <- rndSelect (getAllP (elems e4f2) 10)            (2*testSize)
  hspec $ do
    describe "GalFld.Core.Polynomials Basic" $ 
      it "P[1] == P[1,0]" $
        pList [1] `shouldBe` pList[1,0]
    describe "GalFld.Core.Polynomials @F5 (subset)" $
      subroutine list
    describe "GalFld.Core.Polynomials @e2f2 (full)" $
      subroutine (getAllP (elems e2f2) 4)
    --describe "GalFld.Core.Polynomials @e2e2f2"    $ subroutine vList
    describe "GalFld.Core.Polynomials @e2f2 (subset)" $
      subroutine vList
    {-describe "GalFld.Core.Polynomials @e4f2 (subset)" $-}
      {-subroutine wList-}
