--------------------------------------------------------------------------------
-- |
-- Module      : Sandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von FiniteFields gedacht.
--
--------------------------------------------------------------------------------

module FFSandbox
  ( F2
  , e2f2Mipo, e2f2
  , e2e2f2Mipo, e2e2f2
  , e4f2Mipo, e4f2
  , e3f3Mipo, e3f3
  , main
  )where
import Projekt.Core

import Test.Hspec
import Control.Exception (evaluate)

pp :: (Show a) => [a] -> IO()
pp =  mapM_ print

ppTex :: (ShowTex a) => [a] -> IO()
ppTex = mapM_ (putStrLn . showTex)

{- F4=E2 als Grad 2 Erweiterung von F2
 -
 - Irreduzibles Polynom von Grad 2 über F2:
 -           x²+x+1
 - Mit einer Nullstelle: u = e2f2
 -
 - Also ist F4=F2(u)
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
e2f2Mipo = P[(2,1::F2),(1,1::F2),(0,1::F2)]
e2f2 = FFElem (P[(1,1::F2)]) e2f2Mipo

{- F16=E2(E2)
 - als Grad 2 Erweiterung von E2 durch MPol x²+x+e2f2
 - Mit einer Nullstelle: e2e2f2
 -}
e2e2f2Mipo = P[(2,one),(1,one),(0,e2f2)]
e2e2f2 = FFElem (P[(1,one)]) e2e2f2Mipo

{- F16=E4
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle: e4f2
 -}
e4f2Mipo = P[(4,1::F2),(1,1::F2),(0,1::F2)]
e4f2 = FFElem (P[(1,1::F2)]) e4f2Mipo

--------------------------------------------------------------------------------
-- grundlegende Rechnungen rendern
uElemsTestAdd i j = renderRawTex
  (showTex (elems e2f2!!i) ++ " \\\\\\qquad+ "
  ++ showTex (elems e2f2!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems e2f2!!i + elems e2f2!!j))

uElemsTestMult i j = renderRawTex
  (showTex (elems e2f2!!i) ++ " \\\\\\qquad\\cdot "
  ++ showTex (elems e2f2!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems e2f2!!i * elems e2f2!!j))

vElemsTestAdd i j = renderRawTex
  (showTex (elems e2e2f2!!i) ++ " \\\\\\qquad+ "
  ++ showTex (elems e2e2f2!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems e2e2f2!!i + elems e2e2f2!!j))

vElemsTestMult i j = renderRawTex
  (showTex (elems e2e2f2!!i) ++ " \\\\\\qquad\\cdot "
  ++ showTex (elems e2e2f2!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems e2e2f2!!i * elems e2e2f2!!j))

--------------------------------------------------------------------------------
--  Char 3
{- Irred vom grad 3 öber F3:
 - x³ + 2x + 1
 - x³ + 2x + 2                  <- ausgewählt
 - x³ + x² + 2
 - x³ + x² + x + 2
 - x³ + x² + 2x + 1
 - x³ + 2x² + 1
 - x³ + 2x² + x + 1
 - x³ + 2x² + 2x + 2
 -}
e3f3Mipo = P[(3,1::F3),(1,2::F3),(0,2::F3)]
e3f3 = FFElem (P[(1,1::F3)]) e3f3Mipo

--------------------------------------------------------------------------------
allUnique xs = not $
  or [allUnique' (reverse $ take i xs) | i <- [2..(length xs - 1)]]
    where allUnique' (x:xs) = or [x == y | y <- xs]

subroutine a aMipo = do
  it "test for neutral element" $ mapM_
    (\ x -> x * one `shouldBe` x) (elems a)
  it "x/0 throws exception" $ do
    evaluate (one / FFElem (P[]) aMipo) `shouldThrow` anyException
    evaluate (a / FFElem (P[]) aMipo) `shouldThrow` anyException
  it "0/0 throws exception" $
    evaluate (FFElem (P[]) aMipo / FFElem (P[]) aMipo) `shouldThrow` anyException
  it "1^{-1} == 1" $
    recip one + FFElem (P []) aMipo `shouldBe` one
  it "test (x-x=0)" $ mapM_
    (\ x -> x - x `shouldBe` zero) (elems a)
  it "test (x+x=2*x)" $ mapM_
    (\ x -> x + x `shouldBe` x * 2) (elems a)
  it "+ is bijektiv" $ 
    and [allUnique [x + y | y <- elems a] | x <- elems a]
  it "* is bijektiv" $ 
    and [allUnique [x * y | y <- elems a] | x <- units a]
  it "test recip (full)" $ mapM_
    (\ x -> recip x `shouldBe` head [y | y <- units a, x * y == one]) (units a)
  {-
  it "test recip (x/x=1)" $ mapM_
    (\ x -> x / x `shouldBe` one) (units a)
  it "test recip (x/x*x=x)" $ mapM_
    (\ x -> x / x * x `shouldBe` x) (units a)
  it "test recip (x*x/x=x)" $ mapM_
    (\ x -> x * x / x `shouldBe` x) (units a)
   -}

main :: IO ()
main = hspec $ do
  describe "Projekt.Core.FiniteFields @e2f2: E2 over F2" $
    subroutine e2f2 e2f2Mipo
    --it "e2f2^i erzeugt alle Elemente" $ allUnique [e2f2^i | i <- [0..3]]

  describe "Projekt.Core.FiniteFields @e2e2f2: E2 over E2 over F2" $
    subroutine e2e2f2 e2e2f2Mipo
    --it "e2e2f2^i erzeugt alle Elemente" $ allUnique [e2e2f2^i | i <- [0..15]]

  describe "Projekt.Core.FiniteFields @e4f2: E4 over F2" $
    subroutine e4f2 e4f2Mipo
    --it "e4f2^i erzeugt alle Elemente" $ allUnique [e4f2^i | i <- [0..15]]

  describe "Projekt.Core.FiniteFields @e3f3: E3 over F3" $
    subroutine e3f3 e3f3Mipo
    --it "a^i erzeugt alle Elemente" $ allUnique [a^i | i <- [0..26]]
