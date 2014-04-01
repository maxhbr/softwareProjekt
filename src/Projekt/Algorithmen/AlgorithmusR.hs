
module AlgotrithmusR where
import Projekt.Core.FiniteFields
import Projekt.Core.Polynomials


algR :: (Fractional a) => Polynom a -> Polynom a -> Polynom a
algR g h | f == one   = g
         | otherwise = algR (divP g c) c
  where c = ggTP g h
