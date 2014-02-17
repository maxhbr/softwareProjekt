--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.FiniteFields
-- Note        : Allgemeine implementierung endlicher Körper
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.FiniteFields
  (
  ) where
import Projekt.Core.FiniteField
import Projekt.Core.PrimeFields

--------------------------------------------------------------------------------
--  Beispiel

{- F4=E2 als Grad 2 Erweiterung von Z2
 -
 - Irreduzibles Polynom von Grad 2 über Z2:
 -           x^2+x+1
 - Mit einer Nullstelle:
 -           v
 -
 - Also ist F4=Z2(v)
 -
 - Tabellen:
 -           +   | 0   | 1   | v   | v+1                 *   | 1   | v   | v+1
 -           ----+-----+-----+-----+-----                ----+-----+-----+-----
 -           0   | 0   | 1   | v   | v+1                 1   | 1   | v   | v+1
 -           ----+-----+-----+-----+-----                ----+-----+-----+-----
 -           1   | 1   | 0   | v+1 | v                   v   | v   | v+1 | 1
 -           ----+-----+-----+-----+-----                ----+-----+-----+-----
 -           v   | v   | v+1 | 0   | 1                   v+1 | v+1 | 1   | v
 -           ----+-----+-----+-----+-----
 -           v+1 | v+1 | v   | 1   | 0
 -}
