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
 -           x²+x+1
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

{- F8=E4 als Grad 4 Erweiterung von Z2
 - durck MPol x⁴+x²+1
 -
 - oder
 - als Grad 2 Erweiterung con E2 durch MPol x²+x+1
 -}


--------------------------------------------------------------------------------
--  Weiteres Beispiel

{-
 - Z[X,Y,Z]/
 -        /                                  = GF(3²⁷) = F3^27
 -       /ideal(3,x³-x-1,y³-y+x²,z³-z+x²y²)
 -}
