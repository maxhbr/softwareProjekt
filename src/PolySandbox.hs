--------------------------------------------------------------------------------
-- |
-- Module      : PolySandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von Polynomials gedacht.
--
--------------------------------------------------------------------------------

module PolySandbox where
import Projekt.Core

import FFSandbox (e2f2,e2e2f2,e4f2)
import SpecCommon

--------------------------------------------------------------------------------
--  Über den ganzen Zahlen
exmpPolyInt :: Polynom Integer
exmpPolyInt = aggP $ P[3,2,15,3,345,3434,345,4]

exmpPolyInt' :: Polynom Integer
exmpPolyInt' = aggP $ P[2,5,2345,3,34,3453]

--------------------------------------------------------------------------------
exmpPolyMod = aggP $ P[5::F101,0,0,04::F101,2::F101,5::F101]
exmpPolyMod' = aggP $ P[5::F101,4::F101,0,0,0,0,0,2::F101,5::F101]

--------------------------------------------------------------------------------
exmpPolyMod3  = fromMonomialsP [(0,2::F3), (3,1::F3), (9,1::F3)]

exmpPolyMod5  = P [4::F5, 1, 0, 0, 4, 1]

--------------------------------------------------------------------------------
testSize = 10

unDivP (q,r) a b = a == q * b + r
unEekP (d,s,t) a b = d == s*a + t*b

subroutine e3f3 = do
  it "test * (x*1=x)" $
    pMapM_ (\ x -> x * P[1] `shouldBe` x) e3f3
  it "test divP (x/x=1)" $
    pMapM_ (\ x -> divP x x `shouldBe` (P[1], P[])) e3f3
  it "test divP generally" $
    pMapM_ (\ (x,y) -> unDivP (divP x y) x y `shouldBe` True) $
          zip (take testSize e3f3) (drop testSize e3f3)
  it "x/0 throws exception" $
    pMapM_ (\x -> evaluate (divP x (P[])) `shouldThrow` anyException) e3f3
  it "test eekP" $
    pMapM_ (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True) $
          zip (take testSize e3f3) (drop testSize e3f3)

main :: IO ()
main = do
  list  <- rndSelect (getAllP (units undefined ::[F5]) 4) (2*testSize)
  wList <- rndSelect (getAllP (units e4f2) 4)            (2*testSize)
  hspec $ do
    describe "Projekt.Core.Polynomials @F101 (subset)" $ subroutine list
    describe "Projekt.Core.Polynomials @e2f2 (full)" $
      subroutine (getAllP (units e2f2) 4)
    --describe "Projekt.Core.Polynomials @e2e2f2"    $ subroutine vList
    describe "Projekt.Core.Polynomials @e4f2 (subset)" $ subroutine wList
