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

import FFSandbox (u,v,w)

import Test.Hspec
import Control.Exception (evaluate)

import System.Random
import Control.Monad (replicateM)

rndSelect xs n = do
    gen <- getStdGen
    return $ take n [ xs !! x | x <- randomRs (0, length xs - 1) gen]

--------------------------------------------------------------------------------
--  Über den ganzen Zahlen
exmpPolyInt :: Polynom Integer
exmpPolyInt = aggP $ P [(10,5),(10,4),(3,2),(0,5)]

exmpPolyInt' :: Polynom Integer
exmpPolyInt' = aggP $ P [(8,5),(9,4),(3,2),(0,5)]

--------------------------------------------------------------------------------
exmpPolyMod = aggP $ P [(10,5::Z101),(10,4::Z101),(3,2::Z101),(0,5::Z101)]
exmpPolyMod' = aggP $ P [(8,5::Z101),(9,4::Z101),(3,2::Z101),(0,5::Z101)]

unEekP (d,s,t) a b = d == s*a + t*b
unDivP (q,r) a b = a == q * b + r

--------------------------------------------------------------------------------
testSize = 10

subroutine l = do
  it "test divP (x*1=x)" $ mapM_
    (\ x -> multP x (P[(0,1)]) == x `shouldBe` True)
    l
  it "test divP (x/x=1)" $ mapM_
    (\ x -> divP x x == (P[(0,1)], P[]) `shouldBe` True)
    l
  it "test divP generally" $ mapM_
    (\ (x,y) -> unDivP (divP x y) x y `shouldBe` True) $
    zip (take testSize l) (drop testSize l)
  it "test eekP" $ mapM_
    (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True) $
    zip (take testSize l) (drop testSize l)

main :: IO ()
main = do
  list  <- rndSelect (getAllP (elems undefined ::[Z5]) 4) (2*testSize)
  uList <- rndSelect (getAllP (elems u) 4)               (2*testSize)
  vList <- rndSelect (getAllP (elems v) 2)               (2*testSize)
  wList <- rndSelect (getAllP (elems w) 4)               (2*testSize)
  hspec $ do
    describe "Projekt.Core.Polynomials @Z101" $ subroutine list
    describe "Projekt.Core.Polynomials @u"    $ subroutine uList
    describe "Projekt.Core.Polynomials @v"    $ subroutine vList
    describe "Projekt.Core.Polynomials @w"    $ subroutine wList
