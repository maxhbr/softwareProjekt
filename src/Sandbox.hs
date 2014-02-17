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
type FF = Mod PeanoNumber


exmpPolyMod = aggP $ P [(10,5::FF),(10,4::FF),(3,2::FF),(0,5::FF)]
exmpPolyMod' = aggP $ P [(8,5::FF),(9,4::FF),(3,2::FF),(0,5::FF)]
