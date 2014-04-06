--------------------------------------------------------------------------------
-- |
-- Module      : Sandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von FiniteFields gedacht.
--
--------------------------------------------------------------------------------

module FFSandbox
  ( PF
  , uMipo, u
  , vMipo, v
  , wMipo, w
  , main
  )where
import Projekt.Core

import Test.Hspec
import Control.Exception (evaluate)

pp :: (Show a) => [a] -> IO()
pp =  mapM_ print

ppTex :: (ShowTex a) => [a] -> IO()
ppTex = mapM_ (putStrLn . showTex)

--------------------------------------------------------------------------------
--  Globale Charakteristik
charakteristik :: Integer
charakteristik = 2

--------------------------------------------------------------------------------
--  Definiere Endlichen Körper
data PeanoNumber
instance Numeral PeanoNumber where numValue x = charakteristik
instance Show PeanoNumber    where show       = show
type PF = Mod PeanoNumber

{- F4=E2 als Grad 2 Erweiterung von Z2
 -
 - Irreduzibles Polynom von Grad 2 über Z2:
 -           x²+x+1
 - Mit einer Nullstelle: u
 -
 - Also ist F4=Z2(u)
 -
 - Tabellen:
 -            +  |  0  |  1  |  u  | u+1             *  |  1  |  u  | u+1
 -          -----+-----+-----+-----+-----          -----+-----+-----+-----
 -            0  |  0  |  1  |  u  | u+1             1  |  1  |  u  | u+1
 -          -----+-----+-----+-----+-----          -----+-----+-----+-----
 -            1  |  1  |  0  | u+1 |  u              u  |  u  | u+1 |  1
 -          -----+-----+-----+-----+-----          -----+-----+-----+-----
 -            u  |  u  | u+1 |  0  |  1             u+1 | u+1 |  1  |  u
 -          -----+-----+-----+-----+-----
 -           u+1 | u+1 |  u  |  1  |  0
 -}
uMipo = P [(2,1::PF),(1,1::PF),(0,1::PF)]
u = FFElem (P[(1,1::PF)]) uMipo

{- F16=E2(E2)
 - als Grad 2 Erweiterung von E2 durch MPol x²+x+u
 - Mit einer Nullstelle: v
 -}
vMipo = P [(2,one),(1,one),(0,u)]
v = FFElem (P [(1,one)]) vMipo

{- F16=E4
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle: w
 -}
wMipo = P[(4,1::PF),(1,1::PF),(0,1::PF)]
w = FFElem (P[(1,1::PF)]) wMipo

--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
    describe "Projekt.Core.FiniteFields @u" $ do
      it "test for neutral element" $ mapM_
        (\ x -> x * one `shouldBe` x) (elems u)
      it "x/0 throws exception" $ do
        evaluate (one / FFElem (P[]) uMipo) `shouldThrow` anyException
        evaluate (u / FFElem (P[]) uMipo) `shouldThrow` anyException
      it "0/0 throws exception" $
        evaluate (FFElem (P[]) uMipo / FFElem (P[]) uMipo) `shouldThrow` anyException
      it "1^{-1} == 1" $
        recip one + FFElem (P []) uMipo `shouldBe` one
      it "test invMod (x/x=1)" $ mapM_
        (\ x -> x / x `shouldBe` one) (units u)
      it "test invMod (x/x*x=x)" $ mapM_
        (\ x -> x / x * x `shouldBe` x) (units u)
      it "test invMod (x*x/x=x)" $ mapM_
        (\ x -> x * x / x `shouldBe` x) (units u)

    describe "Projekt.Core.FiniteFields @v" $ do
      it "test for neutral element" $ mapM_
        (\ x -> x * one `shouldBe` x) (elems v)
      it "x/0 throws exception" $ do
        evaluate (one / FFElem (P[]) vMipo) `shouldThrow` anyException
        evaluate (v / FFElem (P[]) vMipo) `shouldThrow` anyException
      it "0/0 throws exception" $
        evaluate (FFElem (P[]) vMipo / FFElem (P[]) vMipo) `shouldThrow` anyException
      it "1^{-1} == 1" $
        recip one + FFElem (P []) vMipo `shouldBe` one
      it "test invMod (x/x=1)" $ mapM_
        (\ x -> x / x `shouldBe` one) (units v)
      it "test invMod (x/x*x=x)" $ mapM_
        (\ x -> x / x * x `shouldBe` x) (units v)
      it "test invMod (x*x/x=x)" $ mapM_
        (\ x -> x * x / x `shouldBe` x) (units v)

    describe "Projekt.Core.FiniteFields @w" $ do
      it "test for neutral element" $ mapM_
        (\ x -> x * one `shouldBe` x) (elems w)
      it "x/0 throws exception" $ do
        evaluate (one / FFElem (P[]) wMipo) `shouldThrow` anyException
        evaluate (w / FFElem (P[]) wMipo) `shouldThrow` anyException
      it "0/0 throws exception" $
        evaluate (FFElem (P[]) wMipo / FFElem (P[]) wMipo) `shouldThrow` anyException
      it "test invMod (x/x=1)" $ mapM_
        (\ x -> x / x `shouldBe` one) (units w)
      it "test invMod (x/x*x=x)" $ mapM_
        (\ x -> x / x * x `shouldBe` x) (units w)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- render latex exmp:
{-
ffElemsTestAdd i j = renderRawTex
  (showTex (ffElems!!i) ++ " \\\\+ "
  ++ showTex (ffElems!!j) ++ " \\\\= "
  ++ showTex (ffElems!!i + ffElems!!j))

ffElemsTestMult i j = renderRawTex
  (showTex (ffElems!!i) ++ " \\\\ \\cdot "
  ++ showTex (ffElems!!j) ++ " \\\\= "
  ++ showTex (ffElems!!i * ffElems!!j))
 -}

--ff1' = FFElem (P[(0,1::PF)]) uMipo

{-fffVTestMult = v / v - FFElem (P[(0,1)]) vMipo == FFElem (P[]) vMipo-}

{-
-- render latex exmp:
fffElemsTestAdd i j = renderRawTex
  (showTex (fffElems!!i) ++ " \\\\+ "
  ++ showTex (fffElems!!j) ++ " \\\\= "
  ++ showTex (fffElems!!i + fffElems!!j))

fffElemsTestMult i j = renderRawTex
  (showTex (fffElems!!i) ++ " \\\\ \\cdot "
  ++ showTex (fffElems!!j) ++ " \\\\= "
  ++ showTex (fffElems!!i * fffElems!!j))
 -}

