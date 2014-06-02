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
testSize = 30

unDivP (q,r) a b = a == q * b + r
unEekP (d,s,t) a b = d == s*a + t*b

subroutine list = do
  it "test * (x*1=x)" $
    pMapM_ (\ x -> x * (pKonst 1) `shouldBe` x) list
  it "test divP (x/x=1)" $
    pMapM_ (\ x -> divPHensel x x `shouldBe` (pKonst 1, nullP)) list
  it "test divP generally" $
    pMapM_ (\ (x,y) -> unDivP (divPHensel x y) x y `shouldBe` True) $
          zip (take testSize list) (drop testSize list)
  it "test divPHorner <-> divPHensel" $
    pMapM_ (\ (x,y) -> (divPHensel x y) `shouldBe` (divP x y)) $
          zip (take testSize list) (drop testSize list)
  {-it "x/0 throws exception" $-}
    {-pMapM_ (\x -> evaluate (divP x (nullP)) `shouldThrow` anyException) list-}
  {-it "test eekP" $-}
    {-pMapM_ (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True) $-}
          {-zip (take testSize list) (drop testSize list)-}

main :: IO ()
main = do
  list  <- rndSelect (getAllP (units undefined ::[F5]) 6) (2*testSize)
  {-wList <- rndSelect (getAllP (units e4f2) 10)            (2*testSize)-}
  hspec $ do
    describe "Projekt.Core.Polynomials @F5 (subset)" $ subroutine list
    {-describe "Projekt.Core.Polynomials @e2f2 (full)" $-}
      {-subroutine (getAllP (units e2f2) 4)-}
    --describe "Projekt.Core.Polynomials @e2e2f2"    $ subroutine vList
    {-describe "Projekt.Core.Polynomials @e4f2 (subset)" $ subroutine wList-}
