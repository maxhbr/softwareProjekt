--------------------------------------------------------------------------------
-- | 
-- Module      : Project.Core.PrimeFields
-- Note        : Einfache prim Körper
-- 
-- 
-- 
--------------------------------------------------------------------------------
module Project.Core.PrimeFields where

data Z   --Zero
data S a --Succ a


--------------------------------------------------------------------------------
--  Data
data Mod a = Integer deriving (Show)

--instance  Eq (Mod a) where
--  Eq g h = false

{-
-- Umwandlung in Werte
class Numeral a where toInt :: a -> Int

instance Numeral Z where toInt _ = 0
instance (Numeral a) => Numeral (S a) where
    toInt = succ . toInt . pred'
            where pred' :: S a -> a

newtype Mod n = MkMod { unMod :: Int }
    deriving (Show)

modulus :: Mod n -> n
modulus = undefined

instance (Numeral n) => Num (Mod n) where
    MkMod x + MkMod y = MkMod $ (x+y) `mod` toInt (modulus x)

-- Beispiel
type Z5 = Mod (S (S (S (S (S Z)))))
 -}
