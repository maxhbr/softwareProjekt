--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Polynomials
  ( Polynom (P), unP
  -- getter
  , getDegrees, getLcP
  -- Operationen auf Polynomen
  , aggP, degP
  -- unär
  , moniP, reziprokP, deriveP
  -- binär
  , divP, modByP, ggTP, eekP
  -- weiteres
  , evalP, getAllP
  ) where
import Data.List
import GHC.Err (divZeroError)
import qualified Control.Arrow as A

import Projekt.Core.ShowTex

--import Debug.Trace

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = P {unP :: [a]} deriving ()

instance (Eq a, Num a) => Eq (Polynom a) where
  f == g = unP (aggP f) == unP (aggP g)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

instance (Show a, Eq a, Num a) => Show (Polynom a) where
  show (P []) = "0"
  show (P ms) = intercalate "+" $
                (\ss -> [s | s <- reverse ss , s /= ""]) $
                zipWith (curry show') ms [0..]
    where show' :: (Show a, Eq a, Num a) => (a,Int) -> String
          show' (0,_) = ""
          show' (m,i) = show m ++ showExp i
          showExp :: Int -> String
          showExp 0 = ""
          showExp 1 = "·\x1B[04mX\x1B[24m"
          showExp i = "·\x1B[04mX" ++ showExp' (show i) ++ "\x1B[24m"
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
  showTex (P []) = "0"
  showTex (P ms) = intercalate "+" $
                reverse $
                zipWith (curry showTex') ms [0..]
    where showTex' :: (ShowTex a, Eq a, Num a) => (a,Int) -> String
          showTex' (0,_) = ""
          showTex' (m,i) = showTex m ++ showExp i
          showExp :: Int -> String
          showExp 0 = ""
          showExp 1 = "\\cdot{}X"
          showExp i = "\\cdot{}X^{" ++ show i ++ "}"

instance (Num a, Eq a) => Num (Polynom a) where
  (P ms) + (P ns) = P $ addP ms ns
  (P ms) * (P ns) = P $ multP ms ns
  fromInteger i   = P [fromInteger i]
  abs _           = error "Prelude.Num.abs: inappropriate abstraction"
  signum (P ms)   = P $ map signum ms
  negate (P ms)   = P $ map negate ms

addP [] gs          = gs
addP fs []          = fs
addP (f:fs) (g:gs)  = f+g : addP fs gs

multP (f:fs) (g:gs) = f*g : addP (multP [f] gs) (multP fs(g:gs))
multP _ _           = []

-- |Sortiert die Koeffizienten absteigend und fasst passende Koeffizienten
-- zusammen.
aggP :: (Num a, Eq a) => Polynom a -> Polynom a
aggP (P ms) = P $ take l ms
  where l = maximum $ 0:[i+1 | i <- [0..(length ms - 1)] , ms!!i /= 0]

getLcP :: (Num a, Eq a) => Polynom a -> a
getLcP (P[]) = 0
getLcP f     = (last . unP . aggP) f

-- |Nimmt ein Polynom und gibt eine liste der Gräder zurrück.
getDegrees :: (Num a, Eq a) => Polynom a -> [Int]
getDegrees (P ms) = [i | i <- [0..(length ms - 1)] , ms!!i /= 0]

degP :: (Num a, Eq a) => Polynom a -> Int
degP f = (length . unP . aggP) f - 1 -- TODO: -∞ für das Null-Polynom

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen
--
--  Unär:
--

moniP :: (Eq a, Fractional a) => Polynom a -> Polynom a
moniP f = P [m / getLcP f | m <- unP f]

-- |Gibt das reziproke Polynom zurrück
-- TODO: Inverses des konstanten Terms ranmultiplizieren???
reziprokP :: (Num a, Fractional a, Eq a) => Polynom a -> Polynom a
reziprokP (P ms) = moniP $ P $ reverse ms

-- |Nimmt ein Polynom und leitet dieses ab.
deriveP :: (Num a, Eq a) => Polynom a -> Polynom a
deriveP (P [])     = P[]
deriveP (P (_:ms)) = P[m * fromInteger i | (i,m) <- zip [(1::Integer)..] ms]

-- | nimmt a und b und gibt (q,r) zurrück, so dass a = q*b+r
--  Teilen mit Rest durch erweitertem euklidischem Algorithmus
divP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> (Polynom a, Polynom a)
divP a b | a == 0       = (P [], P [])
         | b == 0       = divZeroError
         | degDiff < 0 = (P [], a)
         | otherwise   = A.first (monom +) $ divP newA b
  where degDiff   = degP a - degP b
        lcQuot    = getLcP a / getLcP b
        monom     = shiftP degDiff (P[lcQuot])
        newA      = a - monom * b

-- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.
-- Also Division mit rest und Rüchgabewert ist der Rest.
--
-- mehr Performance durch andere Rechnung?
modByP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
modByP f p = snd $ divP f p


-- |Erweiterter Euklidischer Algorithmus: gibt (d,s,t) zurück mit
--  ggT(a,b) = d = s*a + t*b
eekP :: (Eq a, Fractional a) => Polynom a -> Polynom a
                                          -> (Polynom a, Polynom a, Polynom a)
eekP f g | g == 0     = (moniP f ,P[recip $ getLcP f] ,P[])
         | otherwise = (d,t,s-t*q)
  where (q,r)   = divP f g
        (d,s,t) = eekP g r

-- |Algorithmus für ggT
ggTP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
ggTP f g = (\ (x,_,_) -> x) $ eekP f g

--------------------------------------------------------------------------------
--  Weiteres

{-
-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
evalP :: Num a => a -> Polynom a -> a
evalP x f = sum [evalMonom m | m <- unP f]
  where evalMonom (i,c) = product [x | j <- [1..i]] * c
 -}
evalP = undefined

shiftP :: Num a => Int -> Polynom a -> Polynom a
shiftP j (P ms) = P ([0 | i <- [1..j]] ++ ms)

-- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome bis zu diesem
-- Grad.
getAllP :: (Num a, Eq a) => [a] -> Int -> [Polynom a]
getAllP cs d = map P (css d)
  where css n | n == 1     = [[y] | y <- cs]
              | otherwise = [y:ys | y <- cs, ys <- css (n-1) ]
