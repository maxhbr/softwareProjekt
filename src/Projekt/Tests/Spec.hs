--------------------------------------------------------------------------------
-- 
-- Führt alle Tests aus den Sandboxen aus
-- 
--------------------------------------------------------------------------------
module Prjekt.Tests.Spec
  where
import qualified Projekt.Sandbox.PFSandbox     as PF
import qualified Projekt.Sandbox.PolySandbox   as Poly
import qualified Projekt.Sandbox.FFSandbox     as FF
import qualified Projekt.Sandbox.MatrixSandbox as Matrix
import qualified Projekt.Sandbox.AlgSandbox    as Alg

--------------------------------------------------------------------------------
main :: IO ()
main = do
  PF.main
  Poly.main
  FF.main
  Matrix.main
  Alg.main
