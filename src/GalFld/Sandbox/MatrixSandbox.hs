--------------------------------------------------------------------------------
-- |
-- Module      : MatrixSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen der Matrix Implementierung gedacht.
--
--  Die main Funktion enthält Hspec unit tests.
--
--------------------------------------------------------------------------------

module GalFld.Sandbox.MatrixSandbox
  where
import GalFld.Core hiding (examplePoly, examplePoly')
import GalFld.Core.Matrix
import GalFld.Sandbox.FFSandbox

--------------------------------------------------------------------------------
--  Matrizen

m = fromListsM [ [7::F5, 8::F5, 9::F5]
               , [4::F5, 5::F5, 6::F5]
               , [1::F5, 2::F5, 3::F5] ]
mNosSing = fromListsM [ [7::F5, 8::F5, 9::F5]
                      , [4::F5, 5::F5, 6::F5]
                      , [0::F5, 2::F5, 3::F5] ]
m23 = fromListsM [ [7::F5, 8::F5, 9::F5]
                 , [4::F5, 5::F5, 6::F5] ]
m32 = fromListsM [ [7::F5, 8::F5]
                 , [4::F5, 5::F5]
                 , [1::F5, 2::F5] ]

eye2 = fromListsM [ [1::F5, 0]
                  , [0, 1::F5] ]
eye3 = fromListsM [ [1::F5, 0, 0]
                  , [0, 1::F5, 0]
                  , [0, 0, 1::F5] ]
eye = Mdiag (1::F5)

m'  = fromListsM [ [0::F5, 1, 0,  1, 0 ]
                 , [0, -2, 0,  0, 0 ]
                 , [0,  0, 0,  0, 0 ]
                 , [0,  0, 0, -2, 0 ]
                 , [0,  1, 0,  1, 0 ] ]

m''  = fromListsM [ [0::F5, 0, 1 ]
                  , [0, 1, 0 ]
                  , [0, 1, 0 ] ]

--------------------------------------------------------------------------------
--  Matrizen von Polynomen

mp = fromListsM [ [ pList [0,1::F5], pList [0,0,1::F5] ]
                , [ pList [1::F5], pList [0,1::F5] ] ]
mp2 = fromListsM [ [ pList [0,1::F5], pList [0,0,1::F5] ]
                 , [ pList [0,1::F5], pList [0,1::F5] ] ]
