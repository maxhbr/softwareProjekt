--------------------------------------------------------------------------------
-- | 
-- Module      : Project.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
-- 
-- 
-- 
--------------------------------------------------------------------------------
module Project.Core.Polynomials
  ( Polynom -- Data
  , unP
  -- getter
  , getDegrees
  , getMaxDegree
  -- operations
  , aggCoeffs
  , negatePolynom
  , addPolynoms
  , subtractPolynoms
  , multPolynoms
  , derivePolynom
  , evalPolynom
  ) where
import Data.List

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = P [(Integer,a)] deriving ()

instance (Eq a, Num a) => Eq (Polynom a) where
  f == g = unP (aggCoeffs f) == unP (aggCoeffs g)

instance (Show a,Eq a) => Show (Polynom a) where
  show (P []) = ""
  show (P ((i,c):ms))
    | null ms   = show c ++ "·X^{" ++ show i ++ "}"
    | otherwise = show c ++ "·X^{" ++ show i ++ "} + " ++ show (P ms)

-- |Macht aus einem Polynom wieder eine Liste, also quasi das Gegenteil des
-- Polynom Konstruktors.
unP :: Num a => Polynom a -> [(Integer,a)]
unP (P ms) = ms

--------------------------------------------------------------------------------
--  ein Paar Funktionen

-- |Nimmt ein Paar, welches ein Monom Repräsentiert und gibt den Koeffizienten
-- zurück.
getCoeff :: Num a => (Integer , a) -> a
getCoeff (i,c) = c

-- |Nimmt ein Paar, welches ein Monom Repräsentiert und gibt den Grad zurück.
getDegree :: Num a => (Integer,a) -> Integer
getDegree (i,c) = i

-- |Nimmt ein Polynom und gibt eine unsortierte liste der Gräder zurrück.
getDegrees :: Num a => Polynom a -> [Integer]
getDegrees f = getDegrees' [] [getDegree m | m <- unP f]
  where getDegrees' distinct [] = distinct
        getDegrees' distinct (i:is)
          | i `elem` distinct = getDegrees' distinct is
          | otherwise         = getDegrees' (distinct ++ [i]) is

-- |Gibt zu einem Polynom den maximalen Grad zurrück.
getMaxDegree :: Num a => Polynom a -> Integer
getMaxDegree = maximum . getDegrees

-- |Sortiert die Koeffizienten absteigend und fasst passende Koeffizienten
-- zusammen.
aggCoeffs :: Num a => Polynom a -> Polynom a
aggCoeffs f = P [(i,sum [c | (j,c) <- unP f, j==i]) | i <- degrees]
  where degrees = sortBy (flip compare) $ getDegrees f

-- |Gibt zu einem Polynom das Negative Polynom zurrück.
negatePolynom :: Num a => Polynom a -> Polynom a
negatePolynom f = P (negatePolynom' $ unP f)
  where negatePolynom' :: Num a => [(Integer,a)] -> [(Integer,a)]
        negatePolynom' []         = []
        negatePolynom' ((i,c):ms) = (i,-1 * c) : negatePolynom' ms

-- |Nimmt zwei Polynome und addierte diese zusammen.
addPolynoms :: Num a => Polynom a -> Polynom a -> Polynom a
addPolynoms f g = aggCoeffs $ addPolynoms' f g
-- TODO: vlt keine aggCoeffs um mehr performance zu bekommen?
  where addPolynoms' :: Num a => Polynom a -> Polynom a -> Polynom a
        addPolynoms' f g = P (unP f ++ unP g)

-- |Nimmt zwei Polynome und zieht das zweite von dem ersten ab.
subtractPolynoms :: Num a => Polynom a -> Polynom a -> Polynom a
subtractPolynoms f g = addPolynoms f $ negatePolynom g

-- |Nimmt zwei Polynome und multipliziert diese.
multPolynoms :: Num a => Polynom a -> Polynom a -> Polynom a
multPolynoms (P [])     g = P []
multPolynoms (P (m:ms)) g = addPolynoms (multPolynoms (P ms) g)
                          $ multWithMonom m g
  where multWithMonom :: Num a => (Integer,a) -> Polynom a -> Polynom a
        multWithMonom (i,c) (P g) = P [(i+j,c*c') | (j,c') <- g]

-- |Nimmt ein Polynom und leitet dieses ab.
derivePolynom :: Num a => Polynom a -> Polynom a
derivePolynom = P . derivePolynom' . unP
  where derivePolynom' :: Num a => [(Integer,a)] -> [(Integer,a)]
        derivePolynom' [] = []
        derivePolynom' ((i,c):ms)
          | i == 0     = derivePolynom' ms
          | otherwise = (i - 1 , c * fromIntegral i) : derivePolynom' ms

-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
evalPolynom :: Num a => a -> Polynom a -> a
evalPolynom x f = sum [evalMonom m | m <- unP f]
  where evalMonom (i,c) = product [x | j <- [1..i]] * c

--------------------------------------------------------------------------------
--  Für test Zwecke

examplePoly :: Polynom Integer
examplePoly = aggCoeffs $ P [(10,5),(10,4),(3,2),(0,5)]

examplePoly' :: Polynom Integer
examplePoly' = aggCoeffs $ P [(8,5),(9,4),(3,2),(0,5)]
