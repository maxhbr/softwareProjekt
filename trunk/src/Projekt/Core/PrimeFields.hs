{-# LANGUAGE Rank2Types #-} --, DataKinds #-}
{- or:
 - RankNTypes
 - PolymorphicComponents
 - ExistentialQuantification
 - ScopedTypeVariables
 -}
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
import Prelude hiding (divMod, succ)
import Data.Bits (shift)
import GHC.Err (divZeroError)

--------------------------------------------------------------------------------
--  Peano numbers

data Zero
data Succ a

succ :: a -> Succ a
succ = const undefined

peanoPred :: Succ a -> a
peanoPred = const undefined

class Numeral a where
  numValue :: a -> Integer

instance Numeral Zero where
  numValue = const 0
instance Numeral a => Numeral (Succ a) where
  numValue x = numValue (peanoPred x) + 1

instance Show Zero where
  show = show . numValue
instance Numeral a => Show (Succ a) where
  show = show . numValue

--TODO
{-
toPeano :: Integer -> r
toPeano i = toPeano' i id
  where toPeano' :: Integer -> (forall n. (Numeral n) => n -> r) -> r
        toPeano' 0 k = k (undefined :: Zero)
        toPeano' n k = toPeano' (n-1) (k . succ)
 -}

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
  abs _       = error "Prelude.Num.abs: inappropriate abstraction"
  signum _    = error "Prelude.Num.signum: inappropriate abstraction"
  negate x    = MkMod $ negate $ unMod x

instance (Numeral n) => Eq (Mod n) where
  x == y = unMod x - unMod y `mod` numValue (modulus x) == 0

elements :: (Numeral n) => Mod n -> [Mod n]
elements x = map fromInteger [0.. numValue (modulus x)]

units :: (Numeral n) => Mod n -> [Mod n]
units = tail . elements

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

--invPrimeField :: Numeral a => Mod a -> Mod a
--invPrimeField x = invPrimeField' (unMod x `mod` p) 1 0 p
--  where p = numValue (modulus x)
--        invPrimeField' x x1 x2 p
--          |

-- Algorithm 2.22 aus Guide to Elliptic Curve Cryptography
-- TODO: Finde besseren Namen
{-
divMod :: forall a. Numeral a => Mod a -> Mod a -> Mod a
divMod x y = divMod' (unMod x `mod` p, p, unMod y `mod` p, 0)
  where p = numValue (modulus x)
        divMod' :: (Integer, Integer, Integer, Integer) -> Mod a
        divMod' (0,_,_,_)   = divZeroError -- 0 ist keine Einheit
        divMod' (1,_,x1,_)  = MkMod x1
        divMod' (_,1,_,x2)  = MkMod x2
        divMod' (u,v,x1,x2)
          | newU >= newV = divMod' (newU-newV,newV,newX1-newX2,newX2)
          | otherwise   = divMod' (newU,newV-newU,newX1,newX1-newX2)
            where loop :: (Integer, Integer) -> (Integer, Integer)
                  loop (a,b)
                    | even a && even p = (shift a (-1),shift b (-1))
                    | even a           = (shift a (-1),shift (b+p) (-1))
                    | otherwise        = (a,b)
                  (newU,newX1) = loop (u,x1)
                  (newV,newX2) = loop (v,x2)

invMod :: Numeral a => Mod a -> Mod a
invMod x = divMod x 1
 -}

invMod :: Numeral a => Mod a -> Mod a
invMod x = invMod (unMod x `mod` p,p,1,0)
  where p = numValue (modulus x)
        invMod' :: (Integer, Integer, Integer, Integer)
        invMod' (u,v,x1,x2) = 

testInvMod = do
  print $ show $ invMod (1 :: Z7) == (1::Z7)
  print $ show $ invMod (1 :: Z7) * (1 :: Z7) == (1::Z7)
  let x = 4
  print $ show $ invMod (x :: Z7) * (x :: Z7) == (1::Z7)

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
