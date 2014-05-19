--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Polynomials
  where
import qualified Data.List as L
import qualified Control.Arrow as A
import Data.Maybe
import Data.Vector

import Projekt.Core.ShowTex

import Prelude hiding (length,fromList,maximum,take,map,last,product,tail,
  replicate, sum, (++))

--import Debug.Trace

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = P {unP :: Vector a} deriving ()

instance (Eq a, Num a) => Eq (Polynom a) where
  f == g = unP f == unP g
  {-f == g = unP (aggP f) == unP (aggP g)-}

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- |Polonoe aus einer Liste von Monomen (dargestellt als Tuppel) erzeugen.
-- > O(n)
fromMonomialsP :: (Num a, Eq a) => [(Int,a)] -> Polynom a
fromMonomialsP ms = P $ replicate (max+1) 0 // ms
  where max = L.maximum $ L.map (\(i,x) -> i) ms

fromListP :: [a] -> Polynom a
fromListP = P . fromList

instance (Show a, Eq a, Num a) => Show (Polynom a) where
  {-show (P empty) = "0"-} -- pattern matching auf empty funktioniert nicht!
  show (P ms) = L.intercalate "+" $
                (\ss -> [s | s <- L.reverse ss , s /= ""]) $
                L.zipWith (curry show') (toList ms) [0..]
    where show' :: (Show a, Eq a, Num a) => (a,Int) -> String
          show' (0,_) = ""
          show' (m,i) = show m L.++ showExp i
          showExp :: Int -> String
          showExp 0 = ""
          showExp 1 = "·\x1B[04mX\x1B[24m"
          showExp i = "·\x1B[04mX" L.++ showExp' (show i) L.++ "\x1B[24m"
          showExp' :: String -> String
          showExp' ""     = []
          showExp' (c:cs) = newC : showExp' cs
            where newC | c == '0' = '⁰'
                       | c == '1' = '¹'
                       | c == '2' = '²'
                       | c == '3' = '³'
                       | c == '4' = '⁴'
                       | c == '5' = '⁵'
                       | c == '6' = '⁶'
                       | c == '7' = '⁷'
                       | c == '8' = '⁸'
                       | c == '9' = '⁹'

instance (ShowTex a, Num a, Eq a) => ShowTex (Polynom a) where
  {-show (P empty) = "0"-} -- pattern matching auf empty funktioniert nicht!
  showTex (P ms) = L.intercalate "+" $
                   (\ss -> [s | s <- L.reverse ss , s /= ""]) $
                   L.zipWith (curry showTex') (toList ms) [0..]
    where showTex' :: (ShowTex a, Eq a, Num a) => (a,Int) -> String
          showTex' (0,_) = ""
          showTex' (m,i) = showTex m L.++ showExp i
          showExp :: Int -> String
          showExp 0 = ""
          showExp 1 = "\\cdot{}X"
          showExp i = "\\cdot{}X^{" L.++ show i L.++ "}"

instance (Num a, Eq a) => Num (Polynom a) where
  (P ms) + (P ns) = P $ addP ms ns
  (P ms) * (P ns) = P $ multP ms ns
  fromInteger i   = fromListP $ [fromInteger i]
  abs _           = error "Prelude.Num.abs: inappropriate abstraction"
  signum (P ms)   = P $ map signum ms
  negate (P ms)   = P $ map negate ms

(!!!) :: (Num a) => Polynom a -> Int -> a
(!!!) (P ms) i  | i < length ms  = ms ! i
                | otherwise      = 0
            
addP f g  | length f >= length g  = imap (\i -> \x -> x + (P g)!!!i) f
          | otherwise            = addP g f

multP :: (Num a) => Vector a -> Vector a -> Vector a
multP f g = sum' $ imap (\i -> \x -> multWithMonom f (i,x)) g
  where sum' m:ms = 

multWithMonom :: (Num a) => Vector a -> (Int,a) -> Vector a
multWithMonom f (i,x) = (replicate i 0) ++ map (\y -> x*y) f


-- |Sortiert die Koeffizienten absteigend und fasst passende Koeffizienten
-- zusammen. Nur aufrufen bei Veränderung des Polynoms, d.h. Addition und
-- Multiplikation!!
aggP :: (Num a, Eq a) => Polynom a -> Polynom a
aggP (P ms) = P $ take l ms
  where l = maximum $ findIndices (\x -> x /= 0) ms

getLcP :: (Num a, Eq a) => Polynom a -> a
getLcP f         = (last . unP) f

-- |Nimmt ein Polynom und gibt eine liste der Gräder zurrück.
getDegrees :: (Num a, Eq a) => Polynom a -> [Int]
getDegrees (P ms) = [i | i <- [0..(length ms - 1)] , ms!i /= 0]

-- |Gibt zu einem Polynom den Grad
degP :: (Num a, Eq a) => Polynom a -> Maybe Int
degP f | deg >= 0   = Just deg
       | otherwise = Nothing
  where deg = (length . unP) f - 1


uDegP :: (Num a, Eq a) => Polynom a -> Int
uDegP = fromJust . degP

-- |Gibt zu einem Polynom das Produkt der Koeffizient
prodOfCoeffsP :: Num a => Polynom a -> a
prodOfCoeffsP = product . unP

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen
--
--  Unär:
--

moniP :: (Eq a, Fractional a) => Polynom a -> Polynom a
moniP f = P $ map (\x -> x / lc) $ unP f
  where lc = getLcP f

-- |Gibt das reziproke Polynom zurrück
-- TODO: Inverses des konstanten Terms ranmultiplizieren???
{-reziprokP :: (Num a, Fractional a, Eq a) => Polynom a -> Polynom a-}
{-reziprokP (P ms) = moniP $ P $ reverse ms-}

-- |Nimmt ein Polynom und leitet dieses ab.
deriveP :: (Num a, Eq a) => Polynom a -> Polynom a
deriveP (P ms)     = P $ imap (\i -> \x -> x*(fromInteger $ toInteger i)) 
                                                                      $ tail ms

-- | nimmt a und b und gibt (q,r) zurrück, so dass a = q*b+r
--  Teilen mit Rest durch erweitertem euklidischem Algorithmus
divP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> (Polynom a, Polynom a)
divP a b | a == 0       = (P empty, P empty)
         | b == 0       = error "Division by zero"
         | degDiff < 0 = (P empty, a)
         | otherwise   = A.first (monom +) $ divP newA b
  where degDiff   = (fromJust . degP) a - (fromJust . degP) b
        lcQuot    = getLcP a / getLcP b
        monom     = fromMonomialsP [(degDiff,lcQuot)]
        newA      = a - monom * b

{-divP' :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a-}
{-divP' a b = fst $ divP a b-}

{-(@/) :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a-}
{-(@/) = divP'-}

{--- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.-}
{--- Also Division mit rest und Rüchgabewert ist der Rest.-}
{----}
{--- mehr Performance durch andere Rechnung?-}
{-modByP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a-}
{-modByP f p = snd $ divP f p-}

{--- |Erweiterter Euklidischer Algorithmus: gibt (d,s,t) zurück mit-}
{---  ggT(a,b) = d = s*a + t*b-}
{-eekP :: (Eq a, Fractional a) => Polynom a -> Polynom a-}
                                          {--> (Polynom a, Polynom a, Polynom a)-}
{-eekP f g | g == 0     = (moniP f ,P[recip $ getLcP f] ,P[])-}
         {-| otherwise = (d,t,s-t*q)-}
  {-where (q,r)   = divP f g-}
        {-(d,s,t) = eekP g r-}

{--- |Algorithmus für ggT-}
{-ggTP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a-}
{-ggTP f g = (\ (x,_,_) -> x) $ eekP f g-}

{----------------------------------------------------------------------------------}
{---  Weiteres-}

{--- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.-}
{--- Mittels Horner Schema-}
{-evalP x f = evalP' x (unP f) 0-}
{-evalP' x [] acc = acc-}
{-evalP' x (m:ms) acc = evalP' x ms $ acc*x+m-}

{--- |Multipliziert ein Polynom P ms mit X^j-}
{-shiftP :: Num a => Int -> Polynom a -> Polynom a-}
{-shiftP j (P ms) = P ([0 | i <- [1..j]] ++ ms)-}

{--- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome bis zu diesem-}
{--- Grad.-}
{-getAllP :: (Num a, Eq a) => [a] -> Int -> [Polynom a]-}
{-getAllP cs d = map P (css d)-}
  {-where css n | n == 1     = [[y] | y <- cs]-}
              {-| otherwise = [y:ys | y <- cs, ys <- css (n-1) ]-}

{--- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome mit genau diesem-}
{--- Grad.-}
{-getAllByDegP :: (Num a, Eq a) => [a] -> Int -> [Polynom a]-}
{-getAllByDegP cs d = map P (lcss d)-}
  {-where lcss n | n == 1     = [[y] | y <- cs]-}
               {-| otherwise = [y:ys | y <- [e | e <- cs, e /= 0], ys <- css (n-1) ]-}
        {-css n  | n == 1     = [[y] | y <- cs]-}
               {-| otherwise = [y:ys | y <- cs, ys <- css (n-1) ]-}
