module Projekt.Core.AlgebraicTypes where
import Prelude hiding ((+), (-), negate, (*), (/))

class AbGrp a where
  zero    :: a
  (+),(-) :: a -> a -> a
  negate  :: a -> a

  negate x = zero - x
  x - y    = x + negate y

class (AbGrp a) => Ring a where
  one         :: a
  (*)         :: a -> a -> a
  fromInteger :: Integer -> a

class (Ring a) => GalField a where
  inv          :: a -> a
  (/)          :: a -> a -> a
  elems, units :: [a]
