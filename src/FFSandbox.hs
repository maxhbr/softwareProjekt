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

--------------------------------------------------------------------------------
--  Erzeuger des Erweiterungskörpers F4=E2

ff1 = FFKonst (1::PF)
ffV = FFElem (P[(1,1::PF)]) (P [(2,1::PF),(1,1::PF),(0,1::PF)])

{- F8=E4
 - als Grad 2 Erweiterung con E2 durch MPol x²+x+1
 -}

fff1 = FFKonst ff1
fffV = FFElem (P [(1,ff1)]) (P [(2,ff1),(1,ff1),(0,ff1)])

--------------------------------------------------------------------------------
--  Weiteres

{-
 - Z[X,Y,Z]/
 -        /                                  = GF(3²⁷) = F3^27
 -       /ideal(3,x³-x-1,y³-y+x²,z³-z+x²y²)
 -}
