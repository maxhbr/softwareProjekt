--------------------------------------------------------------------------------
-- |
-- Module      : Sandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von FiniteFields gedacht.
--
--------------------------------------------------------------------------------

module FFSandbox
  ( Z2
  , uMipo, u
  , vMipo, v
  , wMipo, w
  , lMipo, l
  , main
  )where
import Projekt.Core

import Test.Hspec
import Control.Exception (evaluate)

pp :: (Show a) => [a] -> IO()
pp =  mapM_ print

ppTex :: (ShowTex a) => [a] -> IO()
ppTex = mapM_ (putStrLn . showTex)

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
uMipo = P[(2,1::Z2),(1,1::Z2),(0,1::Z2)]
u = FFElem (P[(1,1::Z2)]) uMipo

{- F16=E2(E2)
 - als Grad 2 Erweiterung von E2 durch MPol x²+x+u
 - Mit einer Nullstelle: v
 -}
vMipo = P[(2,one),(1,one),(0,u)]
v = FFElem (P[(1,one)]) vMipo

{- F16=E4
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle: w
 -}
wMipo = P[(4,1::Z2),(1,1::Z2),(0,1::Z2)]
w = FFElem (P[(1,1::Z2)]) wMipo

--------------------------------------------------------------------------------
-- grundlegende Rechnungen rendern
uElemsTestAdd i j = renderRawTex
  (showTex (elems u!!i) ++ " \\\\\\qquad+ "
  ++ showTex (elems u!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems u!!i + elems u!!j))

uElemsTestMult i j = renderRawTex
  (showTex (elems u!!i) ++ " \\\\\\qquad\\cdot "
  ++ showTex (elems u!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems u!!i * elems u!!j))

vElemsTestAdd i j = renderRawTex
  (showTex (elems v!!i) ++ " \\\\\\qquad+ "
  ++ showTex (elems v!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems v!!i + elems v!!j))

vElemsTestMult i j = renderRawTex
  (showTex (elems v!!i) ++ " \\\\\\qquad\\cdot "
  ++ showTex (elems v!!j) ++ " \\\\\\qquad\\qquad= "
  ++ showTex (elems v!!i * elems v!!j))

--------------------------------------------------------------------------------
--  Char 3
{- Irred vom grad 3 öber Z3:
 - x³ + 2x + 1
 - x³ + 2x + 2                  <- ausgewählt
 - x³ + x² + 2
 - x³ + x² + x + 2
 - x³ + x² + 2x + 1
 - x³ + 2x² + 1
 - x³ + 2x² + x + 1
 - x³ + 2x² + 2x + 2
 -}
lMipo = P[(3,1::Z3),(1,2::Z3),(0,2::Z3)]
l = FFElem (P[(1,1::Z3)]) lMipo

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
  describe "Projekt.Core.FiniteFields @u: E2 over Z2" $
    subroutine u uMipo
    --it "u^i erzeugt alle Elemente" $ allUnique [u^i | i <- [0..3]]

  describe "Projekt.Core.FiniteFields @v: E2 over E2 over Z2" $
    subroutine v vMipo
    --it "v^i erzeugt alle Elemente" $ allUnique [v^i | i <- [0..15]]

  describe "Projekt.Core.FiniteFields @w: E4 over Z2" $
    subroutine w wMipo
    --it "w^i erzeugt alle Elemente" $ allUnique [w^i | i <- [0..15]]

  describe "Projekt.Core.FiniteFields @l: E3 over Z3" $
    subroutine l lMipo
    --it "a^i erzeugt alle Elemente" $ allUnique [a^i | i <- [0..26]]
