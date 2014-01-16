module Data.FField.PrimeField where

import Prelude

{-
 -from: http://haskellformaths.blogspot.de/2009/08/finite-fields-part-1.html
 -}

data T2
data T3
data T5
{-...-}

class IntegerAsType a where
   value :: a -> Integer

instance IntegerAsType T2 where value _ = 2
instance IntegerAsType T3 where value _ = 3
instance IntegerAsType T5 where value _ = 5

newtype Fp n = Fp Integer

type F2 = Fp T2
type F3 = Fp T3
type F5 = Fp T5

instance IntegerAsType n => Num (Fp n) where
    Fp x + Fp y = Fp $ (x+y) `mod` p where p = value (undefined :: n)
    Fp x * Fp y = Fp $ (x*y) `mod` p where p = value (undefined :: n)
    fromInteger m = Fp $ m `mod` p where p = value (undefined :: n)
