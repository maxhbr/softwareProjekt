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

import FFSandbox

import Test.Hspec
import Control.Exception (evaluate)

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
main = hspec $
    describe "Projekt.Core.Polynomials" $ do
      it "test eekP over Z101" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (reverse $ getAllP (elems undefined ::[Z5]) 5) $
             getAllP (elems undefined ::[Z5]) 3)
      it "test eekP over u" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (reverse $ getAllP (elems u) 4) $ getAllP (elems u) 3)
      {-
      it "test eekP over v" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (take 3 (zip (reverse $ getAllP (elems v) 4) $ (getAllP (elems v) 2))
       -}
      it "test eekP over w" $ mapM_
        (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
        (zip (reverse $ getAllP (elems w) 4) $ getAllP (elems w) 2)
