--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Polynomials
  -- Data
  ( Polynom (P)
  , unP
  -- getter
  , getDegrees
  -- Operationen auf Polynomen
  , aggP
  -- unär
  , negateP, reziprokP, deriveP
  -- binär
  , addP, subP, multP
  -- weiteres
  , evalP
  ) where
import Prelude hiding ( (/) )
import Data.List

{-import Projekt.Core.FiniteField-}

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = P [(Integer,a)] deriving ()

instance (Eq a, Num a) => Eq (Polynom a) where
  f == g = unP (aggP f) == unP (aggP g)

instance (Show a,Eq a) => Show (Polynom a) where
  show (P []) = ""
  show (P ((i,c):ms))
    | null ms   = show c ++ "·X^{\x1B[01m" ++ show i ++ "\x1B[00m}"
    | otherwise = show c ++ "·X^{\x1B[01m" ++ show i ++ "\x1B[00m} + "
        ++ show (P ms)

instance (Num a, Eq a) => Num (Polynom a) where
  x + y         = aggP $ addP x y
  x * y         = aggP $ multP x y
  fromInteger i = P [(0,fromInteger i)]
  abs _         = error "Prelude.Num.abs: inappropriate abstraction"
  signum _      = error "Prelude.Num.signum: inappropriate abstraction"
  negate        = negateP

-- |Macht aus einem Polynom wieder eine Liste, also quasi das Gegenteil des
-- Polynom Konstruktors.
unP :: Num a => Polynom a -> [(Integer,a)]
unP (P ms) = ms

-- |Sortiert die Koeffizienten absteigend und fasst passende Koeffizienten
-- zusammen.
aggP :: (Num a, Eq a) => Polynom a -> Polynom a
aggP f = P (remZeros [(i,sum [c | (j,c) <- unPf, j==i])
             | i <- sortBy (flip compare) $ getDegrees f])
  where unPf = unP f
        remZeros []     = []
        remZeros (i:is) | snd i == 0 = remZeros is
                        | otherwise = i : remZeros is

getLeadingCoeff :: (Num a, Eq a) => Polynom a -> a
getLeadingCoeff = snd . head . unP . aggP

-- |Nimmt ein Polynom und gibt eine unsortierte liste der Gräder zurrück.
getDegrees :: Num a => Polynom a -> [Integer]
getDegrees f = getDegrees' [] [fst m | m <- unP f]
  where getDegrees' distinct [] = distinct
        getDegrees' distinct (i:is)
          | i `elem` distinct = getDegrees' distinct is
          | otherwise         = getDegrees' (distinct ++ [i]) is

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen
--
--  Unär:
--

-- |Gibt zu einem Polynom das Negative Polynom zurrück.
negateP :: Num a => Polynom a -> Polynom a
negateP f = P (negateP' $ unP f)
  where negateP' :: Num a => [(Integer,a)] -> [(Integer,a)]
        negateP' []         = []
        negateP' ((i,c):ms) = (i,-1 * c) : negateP' ms

-- |Gibt das reziproke Polynom zurrück
-- TODO: Inverses des konstanten Terms ranmultiplizieren???
reziprokP :: Num a => Polynom a -> Polynom a
reziprokP f = P [(maxDeg - i,sum [c | (j,c) <- unPf, j==i])
                   | i <- sortBy (flip compare) degrees]
  where unPf    = unP f
        degrees = getDegrees f
        maxDeg  = maximum degrees

-- |Nimmt ein Polynom und leitet dieses ab.
deriveP :: Num a => Polynom a -> Polynom a
deriveP = P . deriveP' . unP
  where deriveP' :: Num a => [(Integer,a)] -> [(Integer,a)]
        deriveP' [] = []
        deriveP' ((i,c):ms)
          | i == 0     = deriveP' ms
          | otherwise = (i - 1 , c * fromIntegral i) : deriveP' ms

--
--  Binär:
--

-- |Nimmt zwei Polynome und addierte diese zusammen.
addP :: Num a => Polynom a -> Polynom a -> Polynom a
addP f g = P (unP f ++ unP g)

-- |Nimmt zwei Polynome und zieht das zweite von dem ersten ab.
subP :: Num a => Polynom a -> Polynom a -> Polynom a
subP f g = addP f $ negateP g

-- |Nimmt zwei Polynome und multipliziert diese.
multP :: Num a => Polynom a -> Polynom a -> Polynom a
multP (P [])     g = P []
multP (P (m:ms)) g = addP (multP (P ms) g) $ multWithMonom m g
  where multWithMonom :: Num a => (Integer,a) -> Polynom a -> Polynom a
        multWithMonom (i,c) (P g) = P [(i+j,c*c') | (j,c') <- g]


--------------------------------------------------------------------------------
--  teilen mit Rest
--  durch erweitertem euklidischem Algorithmus
{-

--TODO

-- | nimmt a und b und gibt (q,r) zurrück, so dass a = q*b+r
divP :: (Num a, FiniteField a) => Polynom a -> Polynom a -> Polynom a
divP a b | degA > degB = a
         | otherwise   = P $ (degA-degB,lcA/lcB) : unP divP newA b
  where degA = maximum $ getDegrees a
        degB = maximum $ getDegrees b
        lcA  = getLeadingCoeff a
        lcB  = getLeadingCoeff b
        newA = undefined

-- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.
-- Also Division mit rest und Rüchgabewert ist der Rest.
modByP :: Num a => Polynom a -> Polynom a -> Polynom a
modByP = undefined

 -}

--------------------------------------------------------------------------------
--  TODO

-- |Algorithmus für ggT

--------------------------------------------------------------------------------
--  Weiteres

-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
evalP :: Num a => a -> Polynom a -> a
evalP x f = sum [evalMonom m | m <- unP f]
  where evalMonom (i,c) = product [x | j <- [1..i]] * c
