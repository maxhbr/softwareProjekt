{-# LANGUAGE TemplateHaskell #-}
module Projekt.Core
  ( module X
  ) where

import Projekt.Core.FiniteField as X
import Projekt.Core.PrimeFields as X
import Projekt.Core.Polynomials as X
import Projekt.Core.Polynomials.FFT as X
import Projekt.Core.FiniteFields as X
import Projekt.Core.Factorization as X
import Projekt.Core.Matrix as X
import Projekt.Core.ShowTex as X

-- Erzeugen von Primkörpern mittel TH:
{-$(genPrimeField 11 "F11")-}
