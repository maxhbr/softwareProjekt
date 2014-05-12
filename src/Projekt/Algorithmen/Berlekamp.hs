
module Projekt.Algorithmen.Berlekamp where
import Data.Maybe
import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials
import Projekt.Core.Matrix
import Debug.Trace


{-
berlekampMatrix :: (Show a, Fractional a, Num a, FiniteField a) => Polynom a -> Matrix a
berlekampMatrix f = transposeM $ M [red i | i <- [0..(n-1)] ]
  where n     = fromJust $ degP f
        q     = elemCount a
        a     = prodOfCoeffsP f
        red i = take n $ (unP (snd (divP (fromMonomialsP [(i*q,1)]) f))
                              ++ [0*a | i <- [0..]] )
 -}
