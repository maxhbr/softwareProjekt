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

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let testSize = 10
  list <- rndSelect (getAllP (elems undefined ::[Z5]) 4) (2*testSize)
  uList <- rndSelect (getAllP (elems u) 4) (2*testSize)
  vList <- rndSelect (getAllP (elems v) 3) (2*testSize)
  wList <- rndSelect (getAllP (elems w) 4) (2*testSize)
  hspec $
    describe "Projekt.Core.Polynomials" $ do
      it "test eekP over Z101" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (take testSize list) (drop testSize list))
      it "test eekP over u" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (take testSize uList) (drop testSize uList))
      it "test eekP over v" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (take testSize vList) (drop testSize vList))
      it "test eekP over w" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (take testSize wList) (drop testSize wList))
