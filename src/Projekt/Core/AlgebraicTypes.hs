module Projekt.Core.AlgebraicTypes where
{-import Prelude hiding ((+))-}

class AbGrp a where
  zero    :: a
  (+),(-) :: a -> a -> a
  negate  :: a -> a

class (AbGrp a) => Ring a where
  one         :: a
  (*)         :: a -> a -> a
  fromInteger :: Integer -> a

class (Ring a) => GalField a where
  inv          :: a -> a
  (/)          :: a -> a -> a
  elems, units :: [a]