module Sandbox where
import Projekt.Core

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


