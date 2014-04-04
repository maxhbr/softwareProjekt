--------------------------------------------------------------------------------
-- |
-- Module      : Sandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--
--
--------------------------------------------------------------------------------

module FFSandbox3 where
import Projekt.Core hiding (examplePoly, examplePoly')
import Projekt.Algorithmen



--------------------------------------------------------------------------------
--  Globale Charakteristik
charakteristik :: Integer
charakteristik = 3

--------------------------------------------------------------------------------
--  Definiere Endlichen Körper

data PeanoNumber
instance Numeral PeanoNumber where numValue x = charakteristik
instance Show PeanoNumber    where show       = show
type PF = Mod PeanoNumber

{-
 - Z[X,Y,Z]/
 -        /                                  = GF(3²⁷) = F3^27
 -       /ideal(3,x³-x-1,y³-y+x²,z³-z+x²y²)
 -}

ff1 = FFKonst 1

ffX = FFElem (P[(1,1::PF)]) (P[(3,1::PF),(1,-1::PF),(0,-1::PF)])

ffY = FFElem (P[(1,ff1)]) (P[(3,ff1),(1,-ff1),(0,ffX^2)])

-- TODO:
{-ffZ = FFElem (P[(1,ff1)]) (P[(3,ff1),(1,-ff1),(0,FFKonst ffX^2 * ffY^2)])-}
