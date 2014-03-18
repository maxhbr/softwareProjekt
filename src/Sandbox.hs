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

--------------------------------------------------------------------------------
--  Über den ganzen Zahlen
exmpPolyInt :: Polynom Integer
exmpPolyInt = aggP $ P [(10,5),(10,4),(3,2),(0,5)]

exmpPolyInt' :: Polynom Integer
exmpPolyInt' = aggP $ P [(8,5),(9,4),(3,2),(0,5)]

--------------------------------------------------------------------------------
--  Globale Charakteristik
charakteristik :: Integer
charakteristik = 101

--------------------------------------------------------------------------------
--  Definiere Endlichen Körper

data PeanoNumber
instance Numeral PeanoNumber where numValue x = charakteristik
instance Show PeanoNumber    where show       = show
type PF = Mod PeanoNumber

exmpPolyMod = aggP $ P [(10,5::PF),(10,4::PF),(3,2::PF),(0,5::PF)]
exmpPolyMod' = aggP $ P [(8,5::PF),(9,4::PF),(3,2::PF),(0,5::PF)]
