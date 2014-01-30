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

class Number a where
  numValue :: a -> Integer

instance Number Zero where
  numValue = const 0
instance Number a => Number (Succ a) where
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

modulus :: Number a => Mod a -> a
modulus = const undefined

instance (Number n) => Num (Mod n) where
  x + y       = MkMod $ (unMod x + unMod y) `mod` numValue (modulus x)
  x * y       = MkMod $ (unMod x * unMod y) `mod` numValue (modulus x)
  fromInteger = MkMod
  abs         = undefined
  signum 0    = 0

instance (Number n) => Eq (Mod n) where
  x == y = representant x == representant y
    where representant x = unMod x `mod` numValue (modulus x)

type Z2 = Mod Two
type Z3 = Mod Three
type Z5 = Mod Five
type Z7 = Mod Seven
