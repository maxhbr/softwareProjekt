--------------------------------------------------------------------------------
-- |
-- Module      : Sandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--
--
--------------------------------------------------------------------------------

module Sandbox where
import Projekt.Core hiding (examplePoly, examplePoly')
import Projekt.Algorithmen
import Projekt.Core.Matrix
import Projekt.Core.ShowTex

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
 - Mit einer Nullstelle:
 -           v dargestellt durch ffV
 -
 - Also ist F4=Z2(v)
 -
 - Tabellen:
 -            +  |  0  |  1  |  v  | v+1             *  |  1  |  v  | v+1
 -          -----+-----+-----+-----+-----          -----+-----+-----+-----
 -            0  |  0  |  1  |  v  | v+1             1  |  1  |  v  | v+1
 -          -----+-----+-----+-----+-----          -----+-----+-----+-----
 -            1  |  1  |  0  | v+1 |  v              v  |  v  | v+1 |  1
 -          -----+-----+-----+-----+-----          -----+-----+-----+-----
 -            v  |  v  | v+1 |  0  |  1             v+1 | v+1 |  1  |  v
 -          -----+-----+-----+-----+-----
 -           v+1 | v+1 |  v  |  1  |  0
 -}

ffVMipo = P [(2,1::PF),(1,1::PF),(0,1::PF)]

ff1 = FFKonst (1::PF)
ffV = FFElem (P[(1,1::PF)]) ffVMipo

ffElems = elems ffV

-- render latex exmp:
ffElemsTestAdd i j = renderRawTex
  (showTex (ffElems!!i) ++ " \\\\+ "
  ++ showTex (ffElems!!j) ++ " \\\\= "
  ++ showTex (ffElems!!i + ffElems!!j))

ffElemsTestMult i j = renderRawTex
  (showTex (ffElems!!i) ++ " \\\\ \\cdot "
  ++ showTex (ffElems!!j) ++ " \\\\= "
  ++ showTex (ffElems!!i * ffElems!!j))

--ff1' = FFElem (P[(0,1::PF)]) ffVMipo

{- F16=E4
 - als Grad 2 Erweiterung con E2 durch MPol x²+x+1
 - Mit einer Nullstelle dargestellt durch fffV
 -}

fffVMipo = P [(2,ff1),(1,ff1),(0,ffV)]

fff1 = FFKonst $ FFKonst (1::PF)
fffV = FFElem (P [(1,ff1)]) fffVMipo

fffElems = elems fffV

-- render latex exmp:
fffElemsTestAdd i j = renderRawTex
  (showTex (fffElems!!i) ++ " \\\\+ "
  ++ showTex (fffElems!!j) ++ " \\\\= "
  ++ showTex (fffElems!!i + fffElems!!j))

fffElemsTestMult i j = renderRawTex
  (showTex (fffElems!!i) ++ " \\\\ \\cdot "
  ++ showTex (fffElems!!j) ++ " \\\\= "
  ++ showTex (fffElems!!i * fffElems!!j))

{- F16
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle:
 -           w dargestellt durch ffW
 -}
ffWMipo = P[(4,1::PF),(1,1::PF),(0,1::PF)]

--ffW = FFElem (P[(1,1::PF),(0,1::PF)]) ffWMipo
ffW = FFElem (P[(1,1::PF)]) ffWMipo

--------------------------------------------------------------------------------
--  Weiteres

{-
 - Z[X,Y,Z]/
 -        /                                  = GF(3²⁷) = F3^27
 -       /ideal(3,x³-x-1,y³-y+x²,z³-z+x²y²)
 -}



--------------------------------------------------------------------------------
--  Matrizen

m = M $ [[7::PF, 8::PF, 9::PF], [4::PF, 5::PF, 6::PF], [1::PF, 2::PF, 3::PF]]
