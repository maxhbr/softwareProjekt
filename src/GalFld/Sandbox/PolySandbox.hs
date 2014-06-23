--------------------------------------------------------------------------------
-- |
-- Module      : PolySandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von Polynomials gedacht.
--
--  Die main Funktion enthält Hspec unit tests.
--
--------------------------------------------------------------------------------

module GalFld.Sandbox.PolySandbox
  where
import GalFld.Core

import GalFld.Sandbox.FFSandbox (e2f2,e2e2f2,e4f2)

--------------------------------------------------------------------------------
--  Über den ganzen Zahlen
exmpPolyInt :: Polynom Integer
exmpPolyInt = pList [3,2,15,3,345,3434,345,4]

exmpPolyInt' :: Polynom Integer
exmpPolyInt' = pList [2,5,2345,3,34,3453]

--------------------------------------------------------------------------------
exmpPolyMod = pList [5::F101,0,0,04::F101,2::F101,5::F101]
exmpPolyMod' = pList [5::F101,4::F101,0,0,0,0,0,2::F101,5::F101]

--------------------------------------------------------------------------------
exmpPolyMod3  = pTup [(0,2::F3), (3,1::F3), (9,1::F3)]
exmpPolyMod3' = pTup [(0,2::F3), (3,1::F3)]

exmpPolyMod5  = pList [4::F5, 1, 0, 0, 4, 1]
