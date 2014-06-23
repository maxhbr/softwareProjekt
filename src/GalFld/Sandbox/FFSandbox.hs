--------------------------------------------------------------------------------
-- |
-- Module      : Sandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von FiniteFields gedacht.
--
--  Die main Funktion enthält Hspec unit tests.
--
--------------------------------------------------------------------------------

module GalFld.Sandbox.FFSandbox
  ( f2
  , e2f2Mipo, e2f2
  , e2e2f2Mipo, e2e2f2
  , e4f2Mipo, e4f2
  , e2f3Mipo, e2f3
  , e3f3Mipo, e3f3
  , e3e3f3Mipo, e3e3f3
  )where
import GalFld.Core
import GalFld.Algorithmen.Rabin
{-import GalFld.Core.Polynomials.Conway-}

pp :: (Show a) => [a] -> IO()
pp =  mapM_ print

ppTex :: (ShowTex a) => [a] -> IO()
ppTex = mapM_ (putStrLn . showTex)

f2 = 1::F2

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
e2f2Mipo = pList [1::F2,1,1] -- x²+x+1
e2f2 = FFElem (pList [0,1::F2]) e2f2Mipo

{- F16=E2(E2)
 - als Grad 2 Erweiterung von E2 durch MPol x²+x+e2f2
 - Mit einer Nullstelle: e2e2f2
 -}
e2e2f2Mipo = pList [e2f2,one,one] -- x²+x+e2f2
e2e2f2 = FFElem (pList [0,e2f2]) e2e2f2Mipo
--e2e2f2 = FFElem (pList [0,e2f2]) e2e2f2Mipo

{- F16=E4
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle: e4f2
 -}
e4f2Mipo = pList [1::F2,1::F2,0,0,1::F2] -- x⁴+x²+1
e4f2 = FFElem (pList [0,1::F2]) e4f2Mipo

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
 - x² + 1
 - x² + x + 2
 - x² + 2x + 2
 -}
e2f3Mipo = pList [1::F3,0,1::F3]
e2f3 = FFElem (pList [0,1::F3]) e2f3Mipo
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
e3f3Mipo = pList [2::F3,2,0,1] -- x³+2x+2
e3f3 = FFElem (pList [0,1::F3]) e3f3Mipo

{-
 - Z[X,Y,Z]/
 -        /                                  = GF(3²⁷) = F3^27
 -       /ideal(3,x³-x-1,y³-y+x²,z³-z+x²y²)
 -}
e3e3f3Mipo = pList [e3f3^2,2,0,1] -- y³-y+x²
e3e3f3 = FFElem (pList [0,one]) e3e3f3Mipo

e3e3e3f3Mipo = pList [FFKonst (e3f3^2) * (e3e3f3^2),2,0,1] -- z³-z+x²y²
e3e3e3f3 = FFElem (pList [0,one]) e3e3e3f3Mipo
