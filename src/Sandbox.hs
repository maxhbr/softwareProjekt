module Sandbox where
import Projekt.Core hiding (examplePoly, examplePoly')

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


examplePoly = aggP $ P [(10,5::FF),(10,4::FF),(3,2::FF),(0,5::FF)]
examplePoly' = aggP $ P [(8,5::FF),(9,4::FF),(3,2::FF),(0,5::FF)]
