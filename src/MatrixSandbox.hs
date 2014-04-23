--------------------------------------------------------------------------------
-- |
-- Module      : MatrixSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--
--
--------------------------------------------------------------------------------

module MatrixSandbox where
import Projekt.Core hiding (examplePoly, examplePoly')
import Projekt.Algorithmen
import Projekt.Core.Matrix

--------------------------------------------------------------------------------
--  Matrizen

m = M [[7::F5, 8::F5, 9::F5], [4::F5, 5::F5, 6::F5], [1::F5, 2::F5, 3::F5]]
m23 = M [[7::F5, 8::F5, 9::F5], [4::F5, 5::F5, 6::F5]]
m32 = M [[7::F5, 8::F5], [4::F5, 5::F5], [1::F5, 2::F5]]

eye2 = M [[1::F5, 0], [0, 1::F5]]
eye3 = M [[1::F5, 0, 0], [0, 1::F5, 0], [0, 0, 1::F5]]
eye = genDiagM (1::F5)

--------------------------------------------------------------------------------
--  Matrizen von Polynomen

mp = M [[P[(1,1::F5)],P[(2,1::F5)]],[P[(0,1::F5)],P[(1,1::F5)]]]

--------------------------------------------------------------------------------
-- TODO: Test Matrix Multiplikation
