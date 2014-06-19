--------------------------------------------------------------------------------
-- 
-- Führt alle Tests aus den Sandboxen aus
-- 
--------------------------------------------------------------------------------
module Main
  where
import qualified GalFld.Sandbox.PFSandbox     as PF
import qualified GalFld.Sandbox.PolySandbox   as Poly
import qualified GalFld.Sandbox.FFSandbox     as FF
import qualified GalFld.Sandbox.MatrixSandbox as Matrix
import qualified GalFld.Sandbox.AlgSandbox    as Alg

--------------------------------------------------------------------------------
main :: IO ()
main = do
  PF.main
  Poly.main
  FF.main
  Matrix.main
  Alg.main
