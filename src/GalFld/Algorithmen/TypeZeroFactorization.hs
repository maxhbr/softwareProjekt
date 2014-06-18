
module TypeZeroFactorization where
import GalFld.Core.FiniteFields
import GalFld.Core.Polynomials

algR :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
algR g h | h == 1   = g
         | otherwise = algR (fst (divP g c)) c
  where c = ggTP g h

algTyp0 :: (Fractional a) => Polynom a -> [Polynom a]
algTyp0 f | f == 1     = []
          | otherwise = algTyp0 (pThRoot f) ++ [fBar]
  where a    = algR f (fst $ divP f (ggTP f (deriveP f)))
        fBar = fst $ divP g a

pThRoot = undefined
