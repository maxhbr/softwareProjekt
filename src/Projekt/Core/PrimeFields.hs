--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.PrimeFields
-- Note        : Einfache prim Körper
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.PrimeFields
  ( 
  -- Peano Numerale
    Numeral (numValue)
  , Zero
  , Succ
  , Two , Three , Five , Seven
  -- Endliche Körper
  , Mod
  , modulus
  -- Beispiele
  , Z2 , Z3 , Z5 , Z7, Z101
  ) where
{-import Prelude hiding ( (/) )-}
import GHC.Err (divZeroError)
--import Data.Bits (shift)
--
import Projekt.Core.FiniteField

--------------------------------------------------------------------------------
--  Peano numbers

data Zero
data Succ a

--succPeano :: a -> Succ a
--succPeano = const undefined

predPeano :: Succ a -> a
predPeano = const undefined

class Numeral a where
  numValue :: a -> Integer

instance Numeral Zero where
  numValue = const 0
instance Numeral a => Numeral (Succ a) where
  numValue x = numValue (predPeano x) + 1

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
  --deriving (Show)

instance (Numeral n, Show n) => Show (Mod n) where
  show x = "[\x1B[01;32m" ++ show (unMod x) ++ "\x1B[00m%" ++ show (modulus x) ++ "]"

modulus :: Numeral a => Mod a -> Integer
modulus x = numValue $ modulus' x
  where modulus' :: Numeral a => Mod a -> a
        modulus' = const undefined

getRepr :: (Numeral n) => Mod n -> Integer
getRepr x = unMod x `mod` modulus x

instance (Numeral n) => Eq (Mod n) where
  x == y = unMod x - unMod y `mod` modulus x == 0

instance (Numeral n) => Num (Mod n) where
  x + y       = MkMod $ (unMod x + unMod y) `mod` modulus x
  x * y       = MkMod $ (unMod x * unMod y) `mod` modulus x
  fromInteger = MkMod
  abs _       = error "Prelude.Num.abs: inappropriate abstraction"
  signum _    = error "Prelude.Num.signum: inappropriate abstraction"
  negate      = MkMod . negate . unMod

instance (Numeral n) => Fractional (Mod n) where
  recip          = invMod
  fromRational _ = error "inappropriate abstraction"

instance (Numeral n) => FiniteField (Mod n) where
  zero  = MkMod 0
  one   = MkMod 1
  -- TODO
  elems = undefined
{-
  elems = elems' zero

elems' :: (Numeral n) => Mod n -> [Mod n]
elems' x = map fromInteger [0.. (numValue (modulus' x) - 1)]
 -}

-- Inversion mit erweitertem Euklidischem Algorithmus
-- Algorithm 2.20 aus Guide to Elliptic Curve Cryptography
invMod :: Numeral a => Mod a -> Mod a
invMod x = invMod' (unMod x `mod` p,p,one,zero)
  where p = modulus x
        invMod' :: Numeral a => (Integer, Integer, Mod a, Mod a) -> Mod a
        divMod' (0,_,_,_) = divZeroError
        invMod' (u,v,x1,x2)
          | u == 1     = x1
          | otherwise = invMod' (v-q*u,u,x2-fromInteger q*x1,x1)
            where q = v `div` u

-- Möglicherweise besser aber noch nicht korrekt:
-- Algorithm 2.22 aus Guide to Elliptic Curve Cryptography
{-
divMod :: forall a. Numeral a => Mod a -> Mod a -> Mod a
divMod x y = divMod' (unMod x `mod` p, p, unMod y `mod` p, 0)
  where p = numValue (modulus' x)
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

--------------------------------------------------------------------------------
--  Examples

type Z2 = Mod Two
type Z3 = Mod Three
type Z5 = Mod Five
type Z7 = Mod Seven

-- Größere Primzahlen
data Peano101
instance Numeral Peano101 where numValue x = 101
instance Show Peano101    where show       = show
type Z101 = Mod Peano101

--------------------------------------------------------------------------------
--  Some small Tests

testInvMod = do
  print $ show $ invMod (1 :: Z7) == (1::Z7)
  print $ show $ all (\x -> invMod x * x == (1::Z7)) units
