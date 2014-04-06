import qualified PFSandbox
import qualified PolySandbox
import qualified FFSandbox
--import qualified MatrixSandbox

--------------------------------------------------------------------------------
main :: IO ()
main = do
  PFSandbox.main
  PolySandbox.main
  FFSandbox.main
  --MatrixSandbox.main
