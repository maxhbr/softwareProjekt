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
  , moniP, negateP, reziprokP, deriveP
  -- binär
  , addP, subP, multP, divP, modByP, ggTP, eekP
  -- weiteres
  , evalP, getAllP
  ) where
import Data.List
import Debug.Trace

import Projekt.Core.ShowTex

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = P [(Integer,a)] deriving ()

instance (Eq a, Num a) => Eq (Polynom a) where
  f == g = unP (aggP f) == unP (aggP g)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

instance (Show a,Eq a) => Show (Polynom a) where
  show (P []) = "0"
  show (P ((i,c):ms)) | null ms   = show c ++ showExp i
                      | otherwise = show c ++ showExp i ++ " + " ++ show (P ms)
    where showExp :: Integer -> String
          showExp 0 = ""
          showExp 1 = "·\x1B[04mX\x1B[24m"
          showExp i = "·\x1B[04mX" ++ showExp' (show i) ++ "\x1B[24m"
          {- Wähle Formatierung:
           -    True für mit UTF8
           -    False für ohne UTF8
           -}
          showExp' = if' True showExp'a showExp'b
          -- Formatieren mit UTF8
          showExp'a :: String -> String
          showExp'a ""     = []
          showExp'a (c:cs) = newC : showExp'a cs
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
          -- Formatieren ohne UTF8
          showExp'b :: String -> String
          showExp'b s = '^' : s

instance (ShowTex a,Eq a) => ShowTex (Polynom a) where
  showTex (P []) = ""
  showTex (P ((i,c):ms))
    | null ms   = showTex c ++ showExp i
    | otherwise = showTex c ++ showExp i ++ " + " ++ showTex (P ms)
    where showExp :: Integer -> String
          showExp 0 = ""
          showExp 1 = "\\cdot{}X"
          showExp i = "\\cdot{}X^{" ++ show i ++ "}"

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
  where unPf            = unP f
        remZeros []     = []
        remZeros (i:is) | snd i == 0 = remZeros is
                        | otherwise = i : remZeros is

getLcP :: (Num a, Eq a) => Polynom a -> a
getLcP = snd . head . unP . aggP

-- |Nimmt ein Polynom und gibt eine unsortierte liste der Gräder zurrück.
getDegrees :: Num a => Polynom a -> [Integer]
getDegrees f = getDegrees' [] [fst m | m <- unP f]
  where getDegrees' distinct [] = distinct
        getDegrees' distinct (i:is)
          | i `elem` distinct = getDegrees' distinct is
          | otherwise         = getDegrees' (distinct ++ [i]) is

degP :: Num a => Polynom a -> Integer
degP = maximum . getDegrees

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen
--
--  Unär:
--

moniP :: (Eq a, Fractional a) => Polynom a -> Polynom a
moniP f = f * P[(0,recip $ getLcP f)]

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

-- |Multiplikation mit Monom
multWithMonom :: Num a => (Integer,a) -> Polynom a -> Polynom a
multWithMonom (i,c) (P g) = P [(i+j,c*c') | (j,c') <- g]

-- | nimmt a und b und gibt (q,r) zurrück, so dass a = q*b+r
--  Teilen mit Rest durch erweitertem euklidischem Algorithmus
divP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> (Polynom a, Polynom a)
divP (P []) b = (P [], P [])
divP a b | degDiff < 0 = (P [], a)
         | otherwise   = divP' $ divP newA b
  where divP' (q,r) = (P $ (degDiff, lcQuot) : unP q, r)
        degDiff     = degP a - degP b
        lcQuot      = getLcP a / getLcP b
        newA        = aggP $ subP a $ multWithMonom (degDiff,lcQuot) b

-- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.
-- Also Division mit rest und Rüchgabewert ist der Rest.
--
-- mehr Performance durch andere Rechnung?
modByP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
modByP f p = snd $ divP f p

-- |Algorithmus für ggT
--
-- TODO: Ersetzen durch eekP?
ggTP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
ggTP f g = (\ (x,_,_) -> x) $ eekP f g

-- |Erweiterter Euklidischer Algorithmus: gibt (d,s,t) zurück mit
--  ggT(a,b) = d = s*a + t*b
eekP :: (Eq a, Fractional a) => Polynom a -> Polynom a
                                          -> (Polynom a, Polynom a, Polynom a)
eekP f g | g == 0     = (moniP f ,P[(0,recip $ getLcP f)] ,P[(0,0)])
         | otherwise = (d,t,s-t*q)
  where (q,r)   = divP f g
        (d,s,t) = eekP g r

--------------------------------------------------------------------------------
--  Weiteres

-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
evalP :: Num a => a -> Polynom a -> a
evalP x f = sum [evalMonom m | m <- unP f]
  where evalMonom (i,c) = product [x | j <- [1..i]] * c

shiftP (P ms) j = P [(fst m + j, snd m) | m <- ms]

-- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome bis zu diesem
-- Grad.
getAllP :: (Num a, Eq a) => [a] -> Integer -> [Polynom a]
getAllP cs d = map (P . resort . zip [0..]) (css d)
  where css n | n == 1     = [[y] | y <- cs]
              | otherwise = [y:ys | y <- cs, ys <- css (n-1) ]
        resort ms         = reverse [m | m <- ms , snd m /= 0]
