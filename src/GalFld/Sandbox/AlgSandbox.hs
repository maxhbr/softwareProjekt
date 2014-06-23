--------------------------------------------------------------------------------
-- |
-- Module      : AlgSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von FiniteFields gedacht.
--
--  Die main Funktion enthält Hspec unit tests.
--
--------------------------------------------------------------------------------

module GalFld.Sandbox.AlgSandbox
  where

import Data.List

import GalFld.Core
import GalFld.Algorithmen

import GalFld.Sandbox.FFSandbox (f2,e2f2,e2e2f2,e4f2,e4f2Mipo)
import GalFld.Sandbox.PolySandbox

--------------------------------------------------------------------------------
--  Beispiele

{-
 - Beispiel in F3[x]:
 -      f = X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1
 -        = (x+1)(x²+1)³(x+2)⁴
 -}
f=pList [1::F3,0,2,2,0,1,1,0,2,2,0,1]
sqf=[(1,pList [1::F3,1])
    ,(3,pList [1::F3,0,1])
    ,(4,pList [2::F3,1])]


pp :: (Show a) => [a] -> IO()
pp =  mapM_ print

f' = pList [4::F5, 1, 0, 0, 4, 1]

f'' = pList [1::F2, 1, 0, 0,0,1]

f''' = pList [1::F2,0,0,1]

--------------------------------------------------------------------------------
-- | failing f:
--  (1₂·X³+1₂·X+1₂ mod 1₂·X⁴+1₂·X+1₂)·X⁴
-- +(1₂·X          mod 1₂·X⁴+1₂·X+1₂)·X³
-- +(1₂·X²+1₂·X+1₂ mod 1₂·X⁴+1₂·X+1₂)·X²
-- +(1₂            mod 1₂·X⁴+1₂·X+1₂)·X
-- +(1₂·X³+1₂·X²   mod 1₂·X⁴+1₂·X+1₂)
sffFailF = pList $ listFFElem e4f2Mipo [ pList [0::F2,0,1,1]
                                , 1
                                , pList [1::F2,1,1]
                                , pList [0::F2,1]
                                , pList [1::F2,1,0,1] ]

--------------------------------------------------------------------------------
-- |Böses Polynom für Berlekamp (f geht auch, da bFailF ein Teiler von f ist)
bFailF = pList [1::F3,0,1]

bFailF2 = (pList $ listFFElem e4f2Mipo [ pList [0::F2,0,1,1]
                                   , 1
                                   , pList [1::F2,1,1]
                                   , pList [0::F2,1]
                                   , pList [1::F2,1,0,1] ])^2
        * (pList $ listFFElem e4f2Mipo [ pList [0::F2,0,1,1]
                                   , 1
                                   , pList [1::F2,1,0,1] ])
        * (pList $ listFFElem e4f2Mipo [ pList [0::F2,0,1,1]
                                   , 1
                                   , 1
                                   , pList [1::F2,1,0,1] ])
