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

--------------------------------------------------------------------------------
--  Matrizen

m = M [[7::PF, 8::PF, 9::PF], [4::PF, 5::PF, 6::PF], [1::PF, 2::PF, 3::PF]]

eye2 = M [[1::PF, 0], [0, 1::PF]]
eye3 = M [[1::PF, 0, 0], [0, 1::PF, 0], [0, 0, 1::PF]]
