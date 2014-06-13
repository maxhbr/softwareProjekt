{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.PrimeFields
-- Note        : Einfache prim Körper
--
-- Die Charakteristik eines (Prim-)Körpers muss immer eine Primzahl sein!
--
-- Ein Primkörper wird wie Folgt definiert (Am Beispiel F7):
-- Man benötigt einen Datentyp, der sich den Modulus merkt
--      data Numeral7
-- Dieser braucht eine passende Instanz Numeral und eine Instanz Show
--      instance Numeral Numeral7 where {numValue x = 7}
--      instance Show Numeral7 where {show = show}
-- Damik kann man nun den Primkörper definieren
--      type F7 = Mod Numeral7
-- Also zusammengefasst:
{-
data Numeral7
instance Numeral Numeral7 where {numValue x = 7}
instance Show Numeral7 where {show = show}
type F7 = Mod Numeral7
 -}
--
-- Altertiv und mit nur einer Zeile mit TemplateHaskell:
-- Dazu muss die Erweiterung TH durch die folgende Zeile Aktiviert werden:
--      {-# LANGUAGE TemplateHaskell #-}
-- Im Code kann man nun durch
{-
$(genPrimeField 7 "F7")
 -}
-- Den Primkörper mit Charakteristik 7 und Namen F7 erzeugen lassen
--------------------------------------------------------------------------------
module Projekt.Core.PrimeFields
  ( Numeral (..)
  -- Endliche Körper
  , Mod (..)
  , modulus
  -- Beispiele
  , F2 , F3 , F5 , F7, F101
  , genPrimeField
  ) where
import Data.Binary
import Language.Haskell.TH

import Projekt.Core.FiniteField
import Projekt.Core.ShowTex
import Projekt.Core.Polynomials

--------------------------------------------------------------------------------
--  Prime fields

class Numeral a where
  numValue :: a -> Int

{-# RULES "MkMod/unMod"   forall x. MkMod (unMod x) = x #-}
{-# RULES "unMod/MkMod"   forall x. unMod (MkMod x) = x #-}
{-# RULES "MkMod.unMod"   MkMod . unMod = id #-}
{-# RULES "unMod.MkMod"   unMod . MkMod = id #-}
newtype Mod n = MkMod Int

{-# INLINE unMod #-}
unMod :: Mod n -> Int
unMod (MkMod k) = k

instance (Numeral n, Show n) => Show (Mod n) where
  show x = "\x1B[33m" ++ show (getRepr x) ++ "\x1B[39m" ++ showModulus x
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

{-# INLINE getRepr #-}
getRepr :: (Numeral n) => Mod n -> Int
getRepr x = unMod x `mod` modulus x

instance (Numeral n) => Eq (Mod n) where
  {-# INLINE (==) #-}
  x == y = (unMod x - unMod y) `rem` modulus x == 0

instance (Numeral n) => Num (Mod n) where
  {-# INLINE (+) #-} 
  x + y       = add x y 
  {-# INLINE (*) #-} 
  x * y       = mult x y 
  fromInteger = MkMod . fromIntegral
  abs x       = x --error "Prelude.Num.abs: inappropriate abstraction"
  signum _    = error "Prelude.Num.signum: inappropriate abstraction"
  {-# INLINE negate #-}
  negate      = MkMod . negate . unMod

{-# INLINE add #-}
add x y = MkMod $ unMod x + unMod y `rem` modulus x

{-# INLINE mult #-}
mult x y = MkMod $ (unMod x * unMod y) `rem` modulus x 

instance (Numeral n) => FiniteField (Mod n) where
  zero                = MkMod 0
  one                 = MkMod 1
  elems               = const $ elems' one
    where elems' :: (Numeral n) => Mod n -> [Mod n]
          elems' x = map MkMod [0.. (modulus x - 1)]
  charakteristik      = modulus
  elemCount           = modulus
  getReprP f          = 0 * snd (head (p2Tup f))

{-# INLINE modulus #-}
modulus :: Numeral a => Mod a -> Int
modulus x = numValue $ modulus' x
  where modulus' :: Numeral a => Mod a -> a
        modulus' = const undefined

instance (Numeral n) => Fractional (Mod n) where
  recip          = invMod
  fromRational _ = error "inappropriate abstraction"

-- Inversion mit erweitertem Euklidischem Algorithmus
-- Algorithm 2.20 aus Guide to Elliptic Curve Cryptography
invMod :: Numeral a => Mod a -> Mod a
invMod 0 = error "Division by zero"
invMod x = invMod' (unMod x `mod` p,p,one,zero)
  where p = modulus x
        invMod' :: Numeral a => (Int, Int, Mod a, Mod a) -> Mod a
        divMod' (0,_,_,_) = error "Division by zero"
        invMod' (u,v,x1,x2)
          | u == 1     = x1
          | otherwise = invMod' (v-q*u,u,x2-MkMod q*x1,x1)
            where q = v `div` u

-- Zur serialisierung wird eine Instanz vom Typ Binary benötigt
instance (Numeral a) => Binary (Mod a) where
   put (MkMod x) = put x
   get           = do x <- get
                      return $ MkMod x

--------------------------------------------------------------------------------
--  Erzeugen von Primkörpern

-- |Erzeugen von Primkörpern durch TemplateHaskell
genPrimeField :: Integer -> String -> Q [Dec]
genPrimeField p pfName = do
  d <- dataD (cxt []) (mkName mName) [] [] []
  i1 <- instanceD (cxt [])
    (appT (conT ''Numeral) (conT (mkName mName)))
    [funD (mkName "numValue")
      [clause [varP $ mkName "x"] 
        (normalB $ litE $ IntegerL p) [] ] ]
  i2 <- instanceD (cxt [])
    (appT (conT ''Show) (conT (mkName mName)))
    [funD (mkName "show") 
      [clause [] ( normalB $ appsE [varE (mkName "show")] ) [] ] ]
  t <- tySynD (mkName pfName) []
    (appT (conT ''Mod) (conT $ mkName mName))
  return [d,i1,i2,t]
    where mName ='M' : show p
ppQ x = putStrLn =<< runQ ((show . ppr) `fmap` x)

-- Erzeugen von Primkörpern mittels CPP Compiler Befehlen
#define PFInstance(MName,MValue,PFName) \
data MName; \
instance Numeral MName where {numValue x = MValue} ;\
instance Show MName where {show = show} ;\
type PFName = Mod MName

PFInstance(Numeral2,2,F2)
PFInstance(Numeral3,3,F3)
PFInstance(Numeral5,5,F5)
PFInstance(Numeral7,7,F7)
PFInstance(Numeral101,101,F101)
