--------------------------------------------------------------------------------
-- |
-- Module      : PolySandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von Polynomials gedacht.
--
--------------------------------------------------------------------------------

module Projekt.Sandbox.PolySandbox
  where
import Projekt.Core

import Projekt.Sandbox.FFSandbox (e2f2,e2e2f2,e4f2)
import Projekt.Sandbox.SandboxCommon

--------------------------------------------------------------------------------
--  Über den ganzen Zahlen
exmpPolyInt :: Polynom Integer
exmpPolyInt = pList [3,2,15,3,345,3434,345,4]

exmpPolyInt' :: Polynom Integer
exmpPolyInt' = pList [2,5,2345,3,34,3453]

--------------------------------------------------------------------------------
exmpPolyMod = pList [5::F101,0,0,04::F101,2::F101,5::F101]
exmpPolyMod' = pList [5::F101,4::F101,0,0,0,0,0,2::F101,5::F101]

--------------------------------------------------------------------------------
exmpPolyMod3  = pTup [(0,2::F3), (3,1::F3), (9,1::F3)]
exmpPolyMod3' = pTup [(0,2::F3), (3,1::F3)]

exmpPolyMod5  = pList [4::F5, 1, 0, 0, 4, 1]

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
    pMapM_ (\ x -> divPHensel x x `shouldBe` (pKonst 1, nullP)) list
  it "test divP generally" $
    pMapM_ (\ (x,y) -> unDivP (divPHensel x y) x y `shouldBe` True) $
          zip (take testSize list) (drop testSize list)
  it "test divPHorner <-> divPHensel" $
    pMapM_ (\ (x,y) -> divPHensel x y `shouldBe` divP x y) $
          zip (take testSize list) (drop testSize list)
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
  list  <- rndSelect (getAllP (elems undefined ::[F5]) 6) (2*testSize)
  vList <- rndSelect (getAllP (elems e2f2) 4)            (2*testSize)
  wList <- rndSelect (getAllP (elems e4f2) 4)            (2*testSize)
  hspec $ do
    describe "Projekt.Core.Polynomials Basic" $ 
      it "P[1] == P[1,0]" $
        pList [1] `shouldBe` pList[1,0]
    describe "Projekt.Core.Polynomials @F5 (subset)" $
      subroutine list
    describe "Projekt.Core.Polynomials @e2f2 (full)" $
      subroutine (getAllP (elems e2f2) 4)
    --describe "Projekt.Core.Polynomials @e2e2f2"    $ subroutine vList
    describe "Projekt.Core.Polynomials @e2f2 (subset)" $
      subroutine vList
    {-describe "Projekt.Core.Polynomials @e4f2 (subset)" $-}
      {-subroutine wList-}
