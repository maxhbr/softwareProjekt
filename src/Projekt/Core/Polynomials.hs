--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Polynomials
  ( Polynom (..)
  , fromMonomialsP
  -- getter
  , getDegrees, getLcP
  -- Operationen auf Polynomen
  , aggP, degP, uDegP, prodOfCoeffsP
  -- unär
  , moniP, reziprokP, deriveP
  -- binär
  , divP, (@/), modByP, ggTP, eekP
  -- weiteres
  , evalP, hasNs
  , getAllP, getAllPs
  , getAllMonicP, getAllMonicPs
  , divPHorner, divPHorner'
  ) where
import Data.List
import qualified Control.Arrow as A
import Data.Maybe
import Data.Binary

import Projekt.Core.ShowTex

import Debug.Trace

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = P {unP :: [a]} deriving ()

instance (Eq a, Num a) => Eq (Polynom a) where
  {-f == g = unP (aggP f) == unP (aggP g)-}
  f == g = eqP f g

eqP (P ms) (P ns) = eqP' ms ns
  where eqP' [] ns = nullP' ns
        eqP' ms [] = nullP' ms
        eqP' (m:ms) (n:ns) = m==n && eqP' ms ns

nullP (P ms) = nullP' ms
nullP' []     = True
nullP' (m:ms) | m /= 0     = False
              | otherwise = nullP' ms


if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- |Polonoe aus einer Liste von Monomen (dargestellt als Tuppel) erzeugen.
fromMonomialsP :: (Num a, Eq a) => [(Int,a)] -> Polynom a
fromMonomialsP []         = P[]
fromMonomialsP ((i,m):ms) = P ([0 | j <- [1..i]] ++ [m]) + fromMonomialsP ms

instance (Show a, Eq a, Num a) => Show (Polynom a) where
  show (P []) = "0"
  show (P ms) = intercalate "+" $
                (\ss -> [s | s <- reverse ss , s /= ""]) $
                zipWith (curry show') ms [0..]
    where show' :: (Show a, Eq a, Num a) => (a,Int) -> String
          show' (0,_) = ""
          show' (m,0) = show m
          {-show' (1,i) = showExp i-}
          show' (m,i) = show m ++ "·" ++ showExp i
          showExp :: Int -> String
          showExp 0 = ""
          showExp 1 = "\x1B[04mX\x1B[24m"
          showExp i = "\x1B[04mX" ++ showExp' (show i) ++ "\x1B[24m"
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
                   (\ss -> [s | s <- reverse ss , s /= ""]) $
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
  (P ms) * (P ns) = P $ multP'' ms ns
  fromInteger i   = P [fromInteger i]
  abs _           = error "Prelude.Num.abs: inappropriate abstraction"
  signum (P ms)   = P $ map signum ms
  negate (P ms)   = P $ map negate ms

{-# INLINE addP #-}
addP [] gs          = gs
addP fs []          = fs
addP (f:fs) (g:gs)  = f+g : addP fs gs

{-# INLINE multP #-}
multP (f:fs) (g:gs) = f*g : addP (multP [f] gs) (multP fs(g:gs))
multP _ _           = []

{-# INLINE multP' #-}
multP' f []             = []
multP' [] f             = []
multP' f g  | n >= m    = foldl1' addP [ multMonom f i a | (i,a) <- gz]
            | otherwise = multP' g f
  where n  = length f
        m  = length g
        gz = zip [0..] g

{-# INLINE multMonom #-}
-- |Multipliziert f mit a*x^i
multMonom :: Num a => [a] -> Int -> a -> [a]
multMonom f i a  = [0 | i <- [1..i]] ++ map (a*) f

{-# INLINE multMonomP #-}
multMonomP :: Num a => Polynom a -> Int -> a -> Polynom a
multMonomP (P f) i a  = P $ multMonom f i a


-- |Aus Math.Polynomial
{-# INLINE multP'' #-}
multP'' :: (Eq a, Num a) => [a] -> [a] -> [a]
multP''  _  []     = []
multP''  []  _     = []
multP''  xs (y:ys) = foldr mul [] xs
    where
        mul x bs
            | x == 0      = 0 : bs
            | otherwise  = (x * y) : zipSum (map (*x) ys) bs


{-# INLINE zipSum #-}
-- like @zipWith (+)@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipSum :: Num t => [t] -> [t] -> [t]
zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys



{-[># INLINE aggP #<]-}
-- |Entfernt trailing zeros
aggP :: (Num a, Eq a) => Polynom a -> Polynom a
aggP (P ms) = P $ aggP' ms []
  where aggP' [] ns     = []
        aggP' (m:ms) ns | m /= 0     = ns ++ [m] ++ aggP' ms []
                        | otherwise = aggP' ms (ns ++ [m])
{-
 - Alt:
aggP :: (Num a, Eq a) => Polynom a -> Polynom a
aggP (P ms) = P $ take l ms
  where l = maximum $ 0:[i+1 | i <- [0..(length ms - 1)] , ms!!i /= 0]
 -}

{-# INLINE getLcP #-}
getLcP :: (Num a, Eq a) => Polynom a -> a
getLcP (P[]) = 0
getLcP f     = (last . unP . aggP) f

{-# INLINE getDegrees #-}
-- |Nimmt ein Polynom und gibt eine liste der Gräder zurrück.
getDegrees :: (Num a, Eq a) => Polynom a -> [Int]
getDegrees (P ms) = [snd m | m <- zip ms [0..], fst m /= 0]
{-getDegrees (P ms) = [i | i <- [0..(length ms - 1)] , ms!!i /= 0]-}

{-# INLINE degP #-}
-- |Gibt zu einem Polynom den Grad
degP :: (Num a, Eq a) => Polynom a -> Maybe Int
degP f | deg >= 0   = Just deg
       | otherwise = Nothing
  where deg = (length . unP . aggP) f - 1

{-# INLINE uDegP #-}
uDegP :: (Num a, Eq a) => Polynom a -> Int
uDegP = fromJust . degP

-- |Gibt zu einem Polynom das Produkt der Koeffizient
prodOfCoeffsP :: Num a => Polynom a -> a
prodOfCoeffsP = product . unP

instance (Num a, Binary a) => Binary (Polynom a) where
   put (P x) = put x
   get       = do x <- get
                  return $ P x

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen
--
--  Unär:
--

moniP :: (Num a, Eq a, Fractional a) => Polynom a -> Polynom a
moniP (P ms) | last ms == 1 = P ms
             | otherwise   = P [m / getLcP (P ms) | m <- ms]

-- |Gibt das reziproke Polynom zurrück
-- TODO: Inverses des konstanten Terms ranmultiplizieren???
reziprokP :: (Num a, Fractional a, Eq a) => Polynom a -> Polynom a
reziprokP (P ms) = moniP $ P $ reverse ms

-- |Nimmt ein Polynom und leitet dieses ab.
deriveP :: (Num a, Eq a) => Polynom a -> Polynom a
deriveP (P [])     = P[]
deriveP (P (_:ms)) = P[m * fromInteger i | (i,m) <- zip [(1::Integer)..] ms]


{-# INLINE divP #-}
-- | nimmt a und b und gibt (q,r) zurrück, so dass a = q*b+r
--  Teilen mit Rest durch erweitertem euklidischem Algorithmus
divP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> (Polynom a, Polynom a)
divP a b | a == 0       = (P [], P [])
         | b == 0       = error "Division by zero"
         | degDiff < 0 = (P [], a)
         | otherwise   = A.first (monom +) $ divP newA b
  where degDiff   = (fromJust . degP) a - (fromJust . degP) b
        lcQuot    = getLcP a / getLcP b
        monom     = fromMonomialsP [(degDiff,lcQuot)]
        newA      = a - multMonomP b degDiff lcQuot

-- |divP mit Horner Schema
--  siehe http://en.wikipedia.org/wiki/Synthetic_division
divPHorner :: (Eq a, Fractional a) => Polynom a -> Polynom a -> (Polynom a,Polynom a)
divPHorner a b | a == 0        = (P[],P[])
               | b == 0        = error "Division by zero"
               | degDiff <= 0  = (P[],P[])
               | otherwise    = (P $ take degDiff horn, P $ drop degDiff horn)
  where horn = reverse $ divPHorner' bs as 
        degDiff   = uDegP a - uDegP b + 1
        bs = tail $ reverse $ unP $ negate $ moniP b 
        as = reverse $ unP a

divPHorner' divs ff@(f:fs) 
  | length fs == length divs = ff
  | otherwise               = f : (divPHorner' divs hs)
  where hs = zipWith (+) fs $ (map (f*) divs) ++ cycle [0]


{-# INLINE divP' #-}
divP' :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
divP' a b = fst $ divPHorner a b

{-# INLINE (@/) #-}
(@/) :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
(@/) = divP'

{-# INLINE modByP #-}
-- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.
-- Also Division mit rest und Rüchgabewert ist der Rest.
--
-- mehr Performance durch andere Rechnung?
modByP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
modByP f p = snd $ divPHorner f p


{-# INLINE eekP #-}
-- |Erweiterter Euklidischer Algorithmus: gibt (d,s,t) zurück mit
--  ggT(a,b) = d = s*a + t*b
eekP :: (Eq a, Fractional a) => Polynom a -> Polynom a
                                          -> (Polynom a, Polynom a, Polynom a)
eekP f g | g == 0     = (moniP f ,P[recip $ getLcP f] ,P[])
         | otherwise = (d,t,s-t*q)
  where (q,r)   = divP f g
        (d,s,t) = eekP g r

{-# INLINE ggTP #-}
-- |Algorithmus für ggT
ggTP :: (Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
ggTP f g = (\ (x,_,_) -> x) $ eekP f g

--------------------------------------------------------------------------------
--  Weiteres

{-# INLINE evalP #-}
-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
-- Mittels Horner Schema
evalP x f = evalP' x (reverse (unP f)) 0
evalP' x [] acc = acc
evalP' x (m:ms) acc = evalP' x ms $ acc*x+m

hasNs :: (Eq a, Fractional a) => Polynom a -> [a] -> Bool
hasNs f es = not (null [f | e <- es, evalP e f == 0])

--------------------------------------------------------------------------------
--  liste alle möglichen Polynome auf

-- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome bis zu diesem
-- Grad.
-- Das Nullpoylnom (P[]) ist NICHT enthalten
getAllP :: (Num a, Fractional a, Eq a) => [a] -> Int -> [Polynom a]
getAllP es d = [(P . map (e*) . unP) f | f <- getAllMonicP es d
                                       , e <- es , e /= 0]

-- |Nimmt eine Liste und eine Liste von Grädern und erzeugt daraus alle 
-- Polynome deren Gräder in der Liste enthalten sind
getAllPs :: (Num a, Fractional a, Eq a) => [a] -> [Int] -> [Polynom a]
getAllPs es ds = [(P . map (e*) . unP) f | f <- getAllMonicPs es ds
                                         , e <- es , e /= 0]

getAllMonicP :: (Num a, Fractional a, Eq a) => [a] -> Int -> [Polynom a]
getAllMonicP es d = getAllMonicPs es [0..d]

getAllMonicPs :: (Num a, Fractional a, Eq a) => [a] -> [Int] -> [Polynom a]
getAllMonicPs es is = map P $ concat [allMonics i | i <- is]
  where allMonics 0 = [[one]]
        allMonics i = [rs++[one] | rs <- ess (i-1)]
        ess i       | i == 0     = [[y] | y <- es]
                    | otherwise = [y:ys | y <- es, ys <- ess (i-1) ]
        one         = head [e | e <- es, e == 1]
        {-one         = onefy $ head [e | e <- es, e /= 0]-}
        {-onefy x     = x/x-}
