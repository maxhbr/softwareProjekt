module Projekt.Projekt
  ( module X
  ) where
import Projekt.Core as X
import Projekt.Algorithmen as X

-- |Nimmt einen Grad `d` und ein Element `e` eines Endlichen Körpers und bildet
-- über den Endlichen Körper, dass das Element enthält eine Erweiterun von Grad
-- `d`.
-- Das übergebene Element muss "genug" Information enthalten.
extendFFBy :: (Show a, Num a, Fractional a, FiniteField a) => Int -> a -> FFElem a
extendFFBy d e = FFElem (P[0,onefy]) $ findIrred $ getAllMonicPs (elems e) [d]
  where onefy | e == 1     = e
              | otherwise = e / e
