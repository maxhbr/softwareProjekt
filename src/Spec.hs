--------------------------------------------------------------------------------
-- 
-- Führt alle Tests aus
-- 
--------------------------------------------------------------------------------
import qualified PFSandbox
import qualified PolySandbox
import qualified FFSandbox
import qualified MatrixSandbox
import qualified AlgSandbox

--------------------------------------------------------------------------------
main :: IO ()
main = do
  {-PFSandbox.main-}
  {-PolySandbox.main-}
  {-FFSandbox.main-}
  {-MatrixSandbox.main-}
  AlgSandbox.main
