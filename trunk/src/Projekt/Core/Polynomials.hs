module Project.Core.Polynomials
    ( --addPolynom
    --, derivePolynom
    ) where
import Data.List

-- Paare aus Grad Koeffizient
data Polynom a = P [(Integer,a)] deriving (Eq)

instance (Show a,Eq a) => Show (Polynom a) where
  show (P [])         = ""
  show (P ((i,c):ms)) | ms == []   = show c ++ "·X^{" ++ show i ++ "}"
                      | otherwise = show c ++ "·X^{" ++ show i ++ "}+" ++ show (P ms)

unP :: Num a => Polynom a -> [(Integer,a)]
unP (P ms) = ms

getCoeff :: Num a => (Integer , a) -> a
getCoeff (i,c) = c

getDegree :: Num a => (Integer,a) -> Integer
getDegree (i,c) = i

getDegrees :: Num a => Polynom a -> [Integer]
getDegrees f = getDegrees' [] [getDegree m | m <- (unP f)]
  where getDegrees' distinct [] = distinct
        getDegrees' distinct (i:is)
          | elem i distinct = getDegrees' distinct is
          | otherwise       = getDegrees' (distinct ++ [i]) is

getMaxDegree :: Num a => Polynom a -> Integer
getMaxDegree = maximum . getDegrees

aggregateCoeffs :: Num a => Polynom a -> Polynom a
aggregateCoeffs f = P [(i,sum [c | (j,c) <- unP f, j==i]) | i <- (getDegrees f)]

--negatePolynom :: Num a => Polynom a -> Polynom a
--negatePolynom [] = []
--negatePolynom (P ((i,c):ms)) = P ((i,c * (fromIntegral -1)) : (unP . negatePolynom ms))

addPolynoms :: Num a => Polynom a -> Polynom a -> Polynom a
addPolynoms f g = aggregateCoeffs $ addPolynoms' f g
-- vlt keine aggregateCoeffs um mehr performance zu bekommen?

--subtractPolynoms :: Num a => Polynom a -> Polynom a -> Polynom a
--subtractPolynoms f g = addPolynoms f $ negatePolynom g

addPolynoms' :: Num a => Polynom a -> Polynom a -> Polynom a
addPolynoms' f g = P (unP f ++ (unP g))

derivePolynom :: Num a => Polynom a -> Polynom a
derivePolynom = P . derivePolynom' . unP
  where derivePolynom' :: Num a => [(Integer,a)] -> [(Integer,a)]
        derivePolynom' [] = []
        derivePolynom' ((i,c):ms) | i == 0     = derivePolynom' ms
                                  | otherwise = (i-1,c*(fromIntegral i)): (derivePolynom' ms)

evalPolynom :: Num a => a -> Polynom a -> a
evalPolynom x f = sum [evalMonom m | m <- (unP f)]
  where evalMonom (i,c) = product [x | j <- [1..i]] * c

examplePoly :: Polynom Integer
examplePoly = aggregateCoeffs $ P [(10,5),(10,4),(3,2),(0,5)]
