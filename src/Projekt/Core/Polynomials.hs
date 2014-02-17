--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Polynomials
  -- Data
  ( Polynom
  , unP
  -- getter
  , getDegrees
  -- Operationen auf Polynomen
  , aggP
  -- unär
  , negateP
  -- binär
  , addP , subP , multP , deriveP
  , evalP
  -- examples
  , examplePoly, examplePoly'
  ) where
import Data.List

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
    | null ms   = show c ++ "·X^{" ++ show i ++ "}"
    | otherwise = show c ++ "·X^{" ++ show i ++ "} + " ++ show (P ms)

-- |Macht aus einem Polynom wieder eine Liste, also quasi das Gegenteil des
-- Polynom Konstruktors.
unP :: Num a => Polynom a -> [(Integer,a)]
unP (P ms) = ms

-- |Sortiert die Koeffizienten absteigend und fasst passende Koeffizienten
-- zusammen.
aggP :: Num a => Polynom a -> Polynom a
aggP f = P [(i,sum [c | (j,c) <- unP f, j==i])
             | i <- sortBy (flip compare) $ getDegrees f]

--------------------------------------------------------------------------------
--  ein paar Getter

-- |Nimmt ein Polynom und gibt eine unsortierte liste der Gräder zurrück.
getDegrees :: Num a => Polynom a -> [Integer]
getDegrees f = getDegrees' [] [fst m | m <- unP f]
  where getDegrees' distinct [] = distinct
        getDegrees' distinct (i:is)
          | i `elem` distinct = getDegrees' distinct is
          | otherwise         = getDegrees' (distinct ++ [i]) is

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen

-- |Gibt zu einem Polynom das Negative Polynom zurrück.
negateP :: Num a => Polynom a -> Polynom a
negateP f = P (negateP' $ unP f)
  where negateP' :: Num a => [(Integer,a)] -> [(Integer,a)]
        negateP' []         = []
        negateP' ((i,c):ms) = (i,-1 * c) : negateP' ms

-- |Nimmt zwei Polynome und addierte diese zusammen.
addP :: Num a => Polynom a -> Polynom a -> Polynom a
addP f g = aggP $ addP' f g
-- TODO: vlt keine aggP um mehr performance zu bekommen?
  where addP' :: Num a => Polynom a -> Polynom a -> Polynom a
        addP' f g = P (unP f ++ unP g)

-- |Nimmt zwei Polynome und zieht das zweite von dem ersten ab.
subP :: Num a => Polynom a -> Polynom a -> Polynom a
subP f g = addP f $ negateP g

-- |Nimmt zwei Polynome und multipliziert diese.
multP :: Num a => Polynom a -> Polynom a -> Polynom a
multP (P [])     g = P []
multP (P (m:ms)) g = addP (multP (P ms) g) $ multWithMonom m g
  where multWithMonom :: Num a => (Integer,a) -> Polynom a -> Polynom a
        multWithMonom (i,c) (P g) = P [(i+j,c*c') | (j,c') <- g]

-- |Nimmt ein Polynom und leitet dieses ab.
deriveP :: Num a => Polynom a -> Polynom a
deriveP = P . deriveP' . unP
  where deriveP' :: Num a => [(Integer,a)] -> [(Integer,a)]
        deriveP' [] = []
        deriveP' ((i,c):ms)
          | i == 0     = deriveP' ms
          | otherwise = (i - 1 , c * fromIntegral i) : deriveP' ms

--------------------------------------------------------------------------------
--  Weiteres

-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
evalP :: Num a => a -> Polynom a -> a
evalP x f = sum [evalMonom m | m <- unP f]
  where evalMonom (i,c) = product [x | j <- [1..i]] * c

--------------------------------------------------------------------------------
--  Für test Zwecke

examplePoly :: Polynom Integer
examplePoly = aggP $ P [(10,5),(10,4),(3,2),(0,5)]

examplePoly' :: Polynom Integer
examplePoly' = aggP $ P [(8,5),(9,4),(3,2),(0,5)]
