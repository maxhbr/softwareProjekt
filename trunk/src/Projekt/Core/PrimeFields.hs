--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.PrimeFields
-- Note        : Einfache prim Körper
--
--
--
--------------------------------------------------------------------------------
module Project.Core.PrimeFields
  ( Zero
  , Succ
  , Two , Three , Five , Seven
  , Mod
  , modulus
  , Z2 , Z3 , Z5 , Z7
  ) where

--------------------------------------------------------------------------------
--  Peano numbers

data Zero
data Succ a

numPred :: Succ a -> a
numPred = const undefined

--TODO
{-toPeano :: Integer -> Either Zero (Succ a)-}
--toPeano i = toPeano' (i, undefined :: Zero)
--  where toPeano' :: Numeral a => (Integer, a) -> Either Zero (Succ a)
--        toPeano' 0 = Left undefined :: Zero
--        toPeano' i = Right $ toPeano' (i-1 , undefined)

class Numeral a where
  numValue :: a -> Integer

instance Numeral Zero where
  numValue = const 0
instance Numeral a => Numeral (Succ a) where
  numValue x = numValue (numPred x) + 1

-- shortcuts
type Two   = Succ (Succ Zero)
type Three = Succ Two
type Five  = Succ (Succ Three)
type Seven = Succ (Succ Five)

--------------------------------------------------------------------------------
--  Prime fields

newtype Mod n = MkMod { unMod :: Integer }
  deriving (Show)

modulus :: Numeral a => Mod a -> a
modulus = const undefined

getRepr :: (Numeral n) => Mod n -> Integer
getRepr x = unMod x `mod` numValue (modulus x)

--setRepr :: (Numeral n) => Mod n -> Mod n
--setRepr = MkMod getRepr

instance (Numeral n) => Num (Mod n) where
  x + y       = MkMod $ (unMod x + unMod y) `mod` numValue (modulus x)
  x * y       = MkMod $ (unMod x * unMod y) `mod` numValue (modulus x)
  fromInteger = MkMod
  abs         = undefined
  signum 0    = 0
  negate x    = MkMod $ negate $ unMod x

instance (Numeral n) => Eq (Mod n) where
  x == y = getRepr x == getRepr y

--------------------------------------------------------------------------------
--  Operations on prime fields


--------------------------------------------------------------------------------
--  Inversion
--
--  Besser: Algorithm 2.20 aus Guide to Elliptic Curve Cryptography

--Input a \in Mod P
-- y_1=1                    1
-- y_2=0                    2
-- While ( x != 0 )         3
--   x=abs(P/x)             3.1
--   x=P-qx;  P=x;
--   y_2=y_1; v_1=y_2-qy_1
-- Return(y_1)

{-invPrimeField :: Numeral a => Mod a -> Mod a-}
{-invPrimeField x = invPrimeField' (unMod x) 1 0 p-}
  {-where p = numValue (modulus x)-}
        {-invPrimeField' x y1 y2 p -}
          {-| x == 0 = MkMod y1-}
          {-| otherwise = invPrimeField' -}

--------------------------------------------------------------------------------
--  Division

-- v=0                      1
-- While (b > 0)            2
--   While (b_0 = 0)        2.1
--     b=b_2;  a=a/2;       2.1.1
--   if (b >= P)            2.2
--     b=b-P;  a=a-v;       2.2.1
--   else                   2.3
--     b=P-b;  P=b;         2.3.1
--     a=v-a;  y=a          2.3.2
-- Return(v)                3.
--
-- b_o represents the least significant bit (LSB) of b.
{-divPrimeField :: Numeral a => Mod a -> Mod a -> Mod a-}

--------------------------------------------------------------------------------
--  Examples

type Z2 = Mod Two
type Z3 = Mod Three
type Z5 = Mod Five
type Z7 = Mod Seven
