
module Berlekamp where
import Projekt.Core.Polynomials

algR :: (Fractional a) => Polynom a -> Polynom a -> Polynom a
algR g h | f == one   = g
         | otherwise = algR 
  where c = ggTP g h

