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
  , F2 , F3 , F5 , F7, F101
  ) where
import GHC.Exception
import Projekt.Core.FiniteField
import Projekt.Core.ShowTex

--------------------------------------------------------------------------------
--  Peano numbers

data Zero
data Succ a

--succPeano :: a -> Succ a
--succPeano = const undefined

predPeano :: Succ a -> a
predPeano = const undefined

class Numeral a where
  numValue :: a -> Int

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

newtype Mod n = MkMod { unMod :: Int }
  --deriving (Show)

instance (Numeral n, Show n) => Show (Mod n) where
  show x = "\x1B[33m" ++ show (unMod x) ++ "\x1B[39m" ++ showModulus x
    where showModulus :: (Numeral n) => Mod n -> String
          showModulus = showModulus' . show . modulus
          showModulus' :: String -> String
          showModulus' "" = ""
          showModulus' (c:cs) = newC : showModulus' cs
            where newC | c == '0' = '₀'
                       | c == '1' = '₁'
                       | c == '2' = '₂'
                       | c == '3' = '₃'
                       | c == '4' = '₄'
                       | c == '5' = '₅'
                       | c == '6' = '₆'
                       | c == '7' = '₇'
                       | c == '8' = '₈'
                       | c == '9' = '₀'

instance (Numeral n, Show n) => ShowTex (Mod n) where
  showTex x = show (unMod x) ++ "_{" ++ show (modulus x) ++ "}"

getRepr :: (Numeral n) => Mod n -> Int
getRepr x = unMod x `mod` modulus x

instance (Numeral n) => Eq (Mod n) where
  x == y = (unMod x - unMod y) `mod` modulus x == 0

instance (Numeral n) => Num (Mod n) where
  x + y       = MkMod $ (unMod x + unMod y) `mod` modulus x
  x * y       = MkMod $ (unMod x * unMod y) `mod` modulus x
  fromInteger = MkMod . fromIntegral
  abs _       = error "Prelude.Num.abs: inappropriate abstraction"
  signum _    = error "Prelude.Num.signum: inappropriate abstraction"
  negate      = MkMod . negate . unMod

instance (Numeral n) => FiniteField (Mod n) where
  zero           = MkMod 0
  one            = MkMod 1
  elems          = const $ elems' one
  charakteristik = modulus
  elemCount      = modulus

modulus :: Numeral a => Mod a -> Int
modulus x = numValue $ modulus' x
  where modulus' :: Numeral a => Mod a -> a
        modulus' = const undefined

elems' :: (Numeral n) => Mod n -> [Mod n]
elems' x = map MkMod [0.. (modulus x - 1)]

instance (Numeral n) => Fractional (Mod n) where
  recip          = invMod
  fromRational _ = error "inappropriate abstraction"

-- Inversion mit erweitertem Euklidischem Algorithmus
-- Algorithm 2.20 aus Guide to Elliptic Curve Cryptography
invMod :: Numeral a => Mod a -> Mod a
invMod x = invMod' (unMod x `mod` p,p,one,zero)
  where p = modulus x
        invMod' :: Numeral a => (Int, Int, Mod a, Mod a) -> Mod a
        divMod' (0,_,_,_) = throw DivideByZero
        invMod' (u,v,x1,x2)
          | u == 1     = x1
          | otherwise = invMod' (v-q*u,u,x2-MkMod q*x1,x1)
            where q = v `div` u


--------------------------------------------------------------------------------
--  Examples

type F2 = Mod Two
type F3 = Mod Three
type F5 = Mod Five
type F7 = Mod Seven

-- Größere Primzahlen
data Peano101
instance Numeral Peano101 where numValue x = 101
instance Show Peano101    where show       = show
type F101 = Mod Peano101
