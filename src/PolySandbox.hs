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

import Test.Hspec
import Control.Exception (evaluate)

import System.Random
import Control.Monad (replicateM)

rndSelect xs n = do
    gen <- getStdGen
    return $ take n [xs!!x | x <- randomRs (0, length xs - 1) gen]

--------------------------------------------------------------------------------
--  Über den ganzen Zahlen
exmpPolyInt :: Polynom Integer
exmpPolyInt = aggP $ P[(10,5),(10,4),(3,2),(0,5)]

exmpPolyInt' :: Polynom Integer
exmpPolyInt' = aggP $ P[(8,5),(9,4),(3,2),(0,5)]

--------------------------------------------------------------------------------
exmpPolyMod = aggP $ P[(10,5::F101),(10,4::F101),(3,2::F101),(0,5::F101)]
exmpPolyMod' = aggP $ P[(8,5::F101),(9,4::F101),(3,2::F101),(0,5::F101)]

--------------------------------------------------------------------------------
testSize = 10

unDivP (q,r) a b = a == q * b + r
unEekP (d,s,t) a b = d == s*a + t*b

subroutine e3f3 = do
  it "test divP (x*1=x)" $
    mapM_ (\ x -> multP x (P[(0,1)]) `shouldBe` x) e3f3
  it "test divP (x/x=1)" $
    mapM_ (\ x -> divP x x `shouldBe` (P[(0,1)], P[])) e3f3
  it "test divP generally" $
    mapM_ (\ (x,y) -> unDivP (divP x y) x y `shouldBe` True) $
          zip (take testSize e3f3) (drop testSize e3f3)
  it "x/0 throws exception" $
    mapM_ (\x -> evaluate (divP x (P[])) `shouldThrow` anyException) e3f3
  it "test eekP" $
    mapM_ (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True) $
          zip (take testSize e3f3) (drop testSize e3f3)

main :: IO ()
main = do
  list  <- rndSelect (getAllP (elems undefined ::[F5]) 4) (2*testSize)
  uList <- rndSelect (getAllP (elems e2f2) 4)            (2*testSize)
  vList <- rndSelect (getAllP (elems e2e2f2) 2)          (2*testSize)
  wList <- rndSelect (getAllP (elems e4f2) 4)            (2*testSize)
  hspec $ do
    describe "Projekt.Core.Polynomials @F101" $ subroutine list
    describe "Projekt.Core.Polynomials @e2f2"    $ subroutine uList
    --describe "Projekt.Core.Polynomials @e2e2f2"    $ subroutine vList
    describe "Projekt.Core.Polynomials @e4f2"    $ subroutine wList
