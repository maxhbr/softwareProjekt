{-# LANGUAGE CPP, BangPatterns#-}
--------------------------------------------------------------------------------
-- |
-- Module      : Projekt.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
module Projekt.Core.Polynomials
  ( Polynom, pList, pTup, pTupUnsave, pKonst, p2Tup, p2List
  , nullP, isNullP
  -- getter
  , getDegrees, getLcP
  -- Operationen auf Polynomen
  , degP, uDegP
  -- unär
  , moniP, moniLcP, deriveP, reciprocP, reciprocP2
  -- binär
  , divP, (@/), modByP, ggTP, eekP, invHensel, divPHensel
  -- weiteres
  , evalP, hasNs
  , getAllP, getAllPs
  , getAllMonicP, getAllMonicPs
  ) where
import Data.List
import qualified Control.Arrow as A
import Data.Maybe
import Data.Binary
import Data.Ord

import Debug.Trace

import Projekt.Core.ShowTex

--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (Integer,a)
-- dargestellt werden. In der ersten Stelle steht der Grad, in der zweiten der
-- Koeffizient.
data Polynom a = PMS { unPMS :: ![(Int,a)], clean :: !Bool} deriving ()

-- |Das Nullpoylnom
nullP = PMS [] True

-- |Erzeuge ein Polynom aus einer Liste von Koeffizienten
pList :: (Num a, Eq a) => [a] -> Polynom a
pList ms = PMS (list2TupleSave ms) True

-- |Erzeugt ein Polynom aus einer Liste von Monomen
pTup :: (Num a, Eq a) => [(Int,a)] -> Polynom a
pTup ms = cleanP $ PMS ms False

-- |Erzeugt ein Polynom aus einer Liste von Monomen
--  Unsichere Variante: Es wird angenommen, dass die Monome
--  in dem Grade nach absteigend sortierter Reihenfolge auftreten!
pTupUnsave :: [(Int,a)] -> Polynom a
pTupUnsave ms = PMS ms True

-- |Erzeugt ein konstantes Polynom, d.h. ein Polynom von Grad 0
pKonst :: a -> Polynom a
pKonst x = PMS [(0,x)] True

p2Tup :: (Num a, Eq a) => Polynom a -> [(Int,a)]
p2Tup = unPMS . cleanP

p2List :: (Num a, Eq a) => Polynom a -> [a]
p2List = tuple2List . unPMS . cleanP

-- |Lösche (i,0) Paare und sortiere dem Grade nach absteigend
cleanP :: (Num a, Eq a) => Polynom a -> Polynom a
cleanP f@(PMS ms True)  = f
cleanP   (PMS ms False) = PMS (clean' ms) True
  where clean' ms = filter (\(_,m) -> m/=0) $ sortBy (flip (comparing fst)) ms

-- |Erzeugt aus einer Liste von Monomen eine Liste von Koeffizienten.
tuple2List :: (Num a) => [(Int,a)]-> [a]
tuple2List ms = tuple2List' ms
  where tuple2List' [] = []
        tuple2List' ((i,m):ms) = zipSum ([0 | j <- [1..i]] ++ [m]) $ tuple2List' ms

-- |Wandelt eine Liste von Koeffizienten in eine Liste von Monomen.
--  Diese ist bereits dem Grade nach absteigend sortiert.
list2TupleSave :: (Eq a, Num a) => [a] -> [(Int,a)]
list2TupleSave ms = list2Tuple' ms 0
  where list2Tuple' [] n                 = []
        list2Tuple' (m:ms) n | m == 0     = list2Tuple' ms (n+1)
                             | otherwise = (list2Tuple' ms (n+1)) ++ [(n,m)]


instance (Eq a, Num a) => Eq (Polynom a) where
  {-f == g = unP (aggP f) == unP (aggP g)-}
  f == g = eqP f g

eqP :: (Eq a, Num a) => Polynom a -> Polynom a -> Bool
eqP (PMS ms True) (PMS ns True)  = eqP' ms ns 
  where eqP' [] ns = isNullP' ns
        eqP' ms [] = isNullP' ms
        eqP' ((i,m):ms) ((j,n):ns) =  i==j && m==n && eqP' ms ns
eqP f g  = eqP (cleanP f) (cleanP g)

isNullP (PMS ms _) = isNullP' ms
isNullP' []     = True
isNullP' ((i,m):ms) | m /= 0     = False
                  | otherwise = isNullP' ms


if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


instance (Show a, Eq a, Num a) => Show (Polynom a) where
  show (PMS [] _) = "0"
  show (PMS ms True) = show' $ tuple2List ms
    where show' ms = intercalate "+" $
                     (\ss -> [s | s <- reverse ss , s /= ""]) $
                     zipWith (curry show'') ms [0..]
          show'' :: (Show a, Eq a, Num a) => (a,Int) -> String
          show'' (0,_) = ""
          show'' (m,0) = show m
          {-show' (1,i) = showExp i-}
          show'' (m,i) = show m ++ "·" ++ showExp i
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
  show f   = show $ cleanP f

instance (ShowTex a, Num a, Eq a) => ShowTex (Polynom a) where
  showTex (PMS [] _) = "0"
  showTex (PMS ms True) = show' $ tuple2List ms
    where show' ms = intercalate "+" $
                     (\ss -> [s | s <- reverse ss , s /= ""]) $
                     zipWith (curry showTex') ms [0..]
          showTex' :: (ShowTex a, Eq a, Num a) => (a,Int) -> String
          showTex' (0,_) = ""
          showTex' (m,i) = showTex m ++ showExp i
          showExp :: Int -> String
          showExp 0 = ""
          showExp 1 = "\\cdot{}X"
          showExp i = "\\cdot{}X^{" ++ show i ++ "}"
  showTex f = showTex $ cleanP f

instance (Num a, Eq a) => Num (Polynom a) where
  {-# INLINE (+) #-}
  f@(PMS _ _) + g@(PMS _ _) = PMS hs True
    where hs = addPM (unPMS $ cleanP f) (unPMS $ cleanP g)
  {-# INLINE (*) #-}
  f@(PMS _ _) * g@(PMS _ _) = PMS hs True
    where hs = multPM (unPMS $ cleanP f) (unPMS $ cleanP g)
  fromInteger i     = PMS [(0,fromInteger i)] True
  abs _             = error "Prelude.Num.abs: inappropriate abstraction"
  signum _          = error "Prelude.Num.signum: inappropriate abstraction"
  negate (PMS ms b) = PMS ((map . A.second) negate ms) b

{-# INLINE addPM #-}
-- | addiere Polynome in Monomdarstellung, d.h
--   [(Int,a)] wobei die Liste in Int ABSTEIGEND sortiert ist
addPM :: (Eq a,Num a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)]
addPM [] gs          = gs
addPM fs []          = fs
addPM ff@((i,f):fs) gg@((j,g):gs)
  | i==j && c/=0  = (i,f+g) : addPM fs gs
  | i==j && c==0  = addPM fs gs
  | i<j         = (j,g) : addPM ff gs
  | i>j         = (i,f) : addPM fs gg
   where c = f+g


{-# INLINE zipSum #-}
-- like @zipWith (+)@ except that when the end of either list is
-- reached, the rest of the output is the rest of the longer input list.
zipSum :: Num t => [t] -> [t] -> [t]
zipSum xs [] = xs
zipSum [] ys = ys
zipSum (x:xs) (y:ys) = (x+y) : zipSum xs ys


{-# INLINE multPM #-}
-- | Multiplikation von absteigend sortierten [(Int,a)] Listen
multPM :: (Eq a, Num a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)]
multPM  ms  []     = []
multPM  []  ns     = []
multPM  ((i,m):ms) ns  = addPM (multPM' i m ns) (multPM ms ns)

{-# INLINE multPM' #-}
multPM' i m []                     = []
multPM' i m ((j,n):ns) | c == 0     = multPM' i m ns
                       | otherwise = (i+j,c) : multPM' i m ns
  where c = n*m

{-# INLINE multMonomP #-}
-- |Multipliziert f mit x^i
multMonomP :: (Eq a, Num a) => Int -> Polynom a -> Polynom a
multMonomP i (PMS ms b)   = PMS (map (A.first (+i)) ms) b

{-# INLINE multKonstP #-}
multKonstP :: (Eq a, Num a) => a -> Polynom a -> Polynom a
multKonstP a f  = PMS (map (A.second (*a)) ms) True
  where ms = unPMS $ cleanP f


{-# INLINE multPShortDown #-}
-- |Multipliziert 2 Polynome miteinander, bei
--  gleichzeitiger Reduktion mod x^l, d.h.
--  schneidet alle Terme >= x^l ab
multPShortDown :: (Eq a, Num a) => Int -> Polynom a -> Polynom a -> Polynom a
multPShortDown l f g  = PMS c True
  where c = multPM_ShortDown l (unPMS $ cleanP f) (unPMS $ cleanP g)

{-# INLINE multPShortUp #-}
-- |Multipliziert 2 Polynome miteinander, bei
--  gleichzeitiger Reduktion mod x^l, d.h.
--  schneidet alle Terme < x^l ab
multPShortUp :: (Eq a, Num a) => Int -> Polynom a -> Polynom a -> Polynom a
multPShortUp l f g  = PMS c True
  where c = multPM_ShortUp l (unPMS $ cleanP f) (unPMS $ cleanP g)

{-# INLINE multPM_ShortDown #-}
-- | Multiplikation von absteigend sortierten [(Int,a)] Listen
multPM_ShortDown :: (Eq a, Num a) => Int -> [(Int,a)] -> [(Int,a)] -> [(Int,a)]
multPM_ShortDown l ms  []     = []
multPM_ShortDown l []  ns     = []
multPM_ShortDown l ((i,m):ms) ns  
                    = addPM (multPM'_ShortDown l i m ns) 
                            (multPM_ShortDown l ms ns)

{-# INLINE multPM_ShortUp #-}
-- | Multiplikation von absteigend sortierten [(Int,a)] Listen
multPM_ShortUp :: (Eq a, Num a) => Int -> [(Int,a)] -> [(Int,a)] -> [(Int,a)]
multPM_ShortUp l ms  []     = []
multPM_ShortUp l []  ns     = []
multPM_ShortUp l ((i,m):ms) ns  
                    = addPM (multPM'_ShortUp l i m ns) 
                            (multPM_ShortUp l ms ns)


{-# INLINE multPM'_ShortDown #-}
multPM'_ShortDown l i m []  = []
multPM'_ShortDown l i m ((j,n):ns) 
    | c == 0 || k >= l = multPM'_ShortDown l i m ns
    | otherwise      = (k,c) : multPM'_ShortDown l i m ns
  where c = n*m
        k = i+j

{-# INLINE multPM'_ShortUp #-}
multPM'_ShortUp l i m []  = []
multPM'_ShortUp l i m ((j,n):ns) 
    | c == 0 || k < l = multPM'_ShortUp l i m ns
    | otherwise      = (k,c) : multPM'_ShortUp l i m ns
  where c = n*m
        k = i+j

{-# INLINE getLcP #-}
getLcP :: (Num a, Eq a) => Polynom a -> a
getLcP (PMS [] _)    = 0
getLcP (PMS fs True) = snd $ head fs
getLcP f             = getLcP $ cleanP f

{-# INLINE getDegrees #-}
-- |Nimmt ein Polynom und gibt eine liste der Gräder zurrück.
getDegrees :: (Num a, Eq a) => Polynom a -> [Int]
getDegrees (PMS ms _ ) = [fst m | m <- ms]



{-# INLINE degP #-}
-- |Gibt zu einem Polynom den Grad
degP :: (Num a, Eq a) => Polynom a -> Maybe Int
degP f@(PMS [] _)    = Nothing
degP (PMS ms True)   = Just $ fst $ head ms
degP f               = degP $ cleanP f


{-# INLINE uDegP #-}
uDegP :: (Num a, Eq a) => Polynom a -> Int
uDegP = fromJust . degP


instance (Num a, Binary a) => Binary (Polynom a) where
   put (PMS x _) = put $ x
   get       = do x <- get
                  return $ (PMS x False)

--------------------------------------------------------------------------------
--  Funktionen auf Polynomen
--
--  Unär:
--

moniP :: (Num a, Eq a, Fractional a) => Polynom a -> Polynom a
moniP f@(PMS [] _)    = f
moniP f@(PMS ms True) = PMS ns True
  where ns = map (\(i,m) -> (i,m/l)) ms
        l  = snd $ head ms
moniP f               = moniP $ cleanP f

-- |Normiert f und gibt gleichzeitig das Inverse des Leitkoeefizieten zurück
moniLcP :: (Num a, Eq a, Fractional a) => Polynom a -> (a,Polynom a)
moniLcP f@(PMS [] _)    = (0,f)
moniLcP f@(PMS ms True) = (l,PMS ns True)
  where ns = map (\(i,m) -> (i,m*l)) ms
        l  = recip $ snd $ head ms
moniLcP f               = moniLcP $ cleanP f


-- |Nimmt ein Polynom und leitet dieses ab.
deriveP :: (Num a, Eq a) => Polynom a -> Polynom a
deriveP (PMS [] _) = PMS [] True
deriveP (PMS ms b) = PMS (deriveP' ms) b
  where deriveP' [] = []
        deriveP' ((i,m):ms) | j<0       = deriveP' ms
                            | c==0       = deriveP' ms
                            | otherwise = (j,c) : deriveP' ms
          where j=i-1
                c=m*fromInteger (fromIntegral i)


{-# INLINE reciprocP #-}
reciprocP :: (Eq a, Fractional a) => Polynom a -> Polynom a
reciprocP f = reciprocP2 d f
  where d  = uDegP f


reciprocP2 :: (Eq a, Fractional a) => Int -> Polynom a -> Polynom a
reciprocP2 k f = cleanP $ PMS ms False
  where d  = uDegP f
        ms = map (\(i,m) -> (k-i,m)) $ unPMS f

-- |divP mit Horner Schema
--  siehe http://en.wikipedia.org/wiki/Synthetic_division
divP :: (Show a, Eq a, Fractional a) =>
                              Polynom a -> Polynom a -> (Polynom a,Polynom a)
divP a b           = divPHorner a b

divPHorner a (PMS [] _)    = error "Division by zero"
divPHorner a@(PMS as True) b@(PMS bs True)
    | isNullP a        = (PMS [] True,PMS [] True)
    | degDiff <= 0      = (PMS [] True,a)
    | otherwise        = toP $ A.first (map (A.first (\i -> i-degB))) $ 
                                                      splitAt splitPoint horn
  where horn       = divPHornerM' bs as lc degB
        degDiff    = uDegP a - uDegP b + 1
        bs         = tail $ unPMS $ negate b
        as         = unPMS a
        lc         = getLcP b
        degB       = uDegP b
        splitPoint = length [i | (i,j) <- horn, i >= degB]
        toP (a,b)  = (PMS a True, PMS b True)
divPHorner a b = divPHorner (cleanP a) (cleanP b)

-- |Horner für absteigend sortierte [(Int,a)] Paare
divPHornerM' _  [] _ _ = []
divPHornerM' divs ff@((i,f):fs) lc n
  | n > fst (head ff)  = ff
  | otherwise            = --trace ("horner' divs="++show divs++" f="++show f++" f/lc="++show (f/lc)++
     -- " ff="++show ff++"\n-> i="++show i++" n="++show n++" => (i,fbar)="++show (i,fbar)++" hs="++show hs) $
        (i,fbar) : divPHornerM' divs hs lc n
  where fbar = f/lc
        hs   = addPM fs $! js 
        js   = map ( (+) (i-n) A.*** (*) fbar) divs

{-# INLINE divP' #-}
divP' :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
divP' a b = fst $ divP a b

{-# INLINE (@/) #-}
(@/) :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
(@/) = divP'

{-# INLINE modByP #-}
-- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.
-- Also Division mit rest und Rüchgabewert ist der Rest.
--
modByP :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
modByP f p = snd $ divP f p

-- |Hensel inverse lift
--  Input: Polynom h mit h(0) = 1
--         Int l
--  Output: h^(-1) mod x^k
invHensel :: (Show a, Num a, Eq a, Fractional a) => Polynom a -> Int -> Polynom a
invHensel h k  | isNullP h  = nullP
               | otherwise  = invHensel' (pKonst 1) 1 1 [] (unPMS h)
  where invHensel' !a !l !lold !h0 !h1 
          | l >= k     = -- trace ("invHensel' done l="++show l++" a="++show (length $ unPMS a))$ 
                        modMonomP k a
          | otherwise = --trace ("invHensel' l="++show l++" a="++show a
                        {-++ ", h0="++show h0++", h1="++show h1++"\n"-}
                        {-++ ", h0'="++show h0'++", h1'="++show h1'-}
                        {-++ ", a*h0="++show (a*(PMS h0' True))-}
                        {-++ ", c="++show c ++", b="++show b++"\n")$-}
                        invHensel' (a+a') l' l h0' h1'
          where b = negate $ multPShortDown l a $!
                            (multPShortDown l a (PMS h1' True)) + (PMS c True)
                a' = multMonomP l b
                l' = 2*l
                h1' = map (\(i,m) -> (i-lold,m)) $ takeWhile (\(i,_) -> i>=lold) h1
                h0'' = filter (\(i,_) -> i<lold) h1
                h0''' | lold==1 = h0''
                      | otherwise = map (\(i,m) -> (i+lold,m)) h0''
                h0' = h0''' ++ h0
                c   = map (\(i,m) -> (i-l,m)) $ multPM_ShortUp l (unPMS a) h0'



modMonomP :: (Eq a, Num a) => Int -> Polynom a -> Polynom a
modMonomP _ (PMS [] _)    = nullP
modMonomP l (PMS ms True) = PMS (dropWhile (\(i,_) -> i>=l) ms) True
modMonomP l f             = modMonomP l $ cleanP f


divPHensel :: (Show a, Eq a, Fractional a) =>
              Polynom a -> Polynom a -> (Polynom a, Polynom a)
divPHensel a b
    | isNullP a = (nullP, nullP)
    | a == b     = (pKonst 1,nullP)
    | l <= 0     = (nullP,a)
    | otherwise = --trace ("a="++show a++" b="++show b++"\n=>f="++show f++" g="++show g++" test g*f mod x^l ="++show (modMonomP l (g*f))++" q="++show q) $
                  (q',r)
  where n  = uDegP a
        m  = uDegP b
        l  = n-m+1
        (lc,b') = moniLcP b
        f  = reciprocP2 m b'
        g  = invHensel f l
        q  = multPShortDown l g $ reciprocP2 n a
        q' = multKonstP lc $ reciprocP2 (l-1) q
        r  = a - b*q'



{-# INLINE eekP #-}
-- |Erweiterter Euklidischer Algorithmus: gibt (d,s,t) zurück mit
--  ggT(a,b) = d = s*a + t*b
eekP :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a
                                          -> (Polynom a, Polynom a, Polynom a)
eekP f g | g == 0     = (moniP f, PMS [(0,recip $ getLcP f)] True, PMS [] True)
         | otherwise = (d,t,s-t*q)
  where (q,r)   = divP f g
        (d,s,t) = eekP g r

{-# INLINE ggTP #-}
-- |Algorithmus für ggT
ggTP :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
ggTP f g = (\ (x,_,_) -> x) $ eekP f g

--------------------------------------------------------------------------------
--  Weiteres

{-# INLINE evalP #-}
-- |Nimmt einen Wert und ein Polynom umd wertet das Polynom an dem Wert aus.
-- Mittels Horner Schema
evalP :: (Eq a, Num a) => a -> Polynom a -> a
evalP x f = evalP' x (unPMS $ cleanP f)
evalP' :: (Num a) => a -> [(Int,a)] -> a
evalP' x []   = 0
evalP' x fs   = foldl' (\a -> \(_,m) -> a*x+m) 0 fs

hasNs :: (Eq a, Fractional a) => Polynom a -> [a] -> Bool
hasNs f es = not (null [f | e <- es, evalP e f == 0])

hasNs' f es = [evalP e f | e <- es]
--------------------------------------------------------------------------------
--  liste alle möglichen Polynome auf

-- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome bis zu diesem
-- Grad.
-- Das Nullpoylnom (P[]) ist NICHT enthalten
getAllP :: (Num a, Fractional a, Eq a) => [a] -> Int -> [Polynom a]
getAllP es d = [PMS (map (A.second (e*)) $ unPMS f) True | f <- getAllMonicP es d
                                       , e <- es , e /= 0]

-- |Nimmt eine Liste und eine Liste von Grädern und erzeugt daraus alle
-- Polynome deren Gräder in der Liste enthalten sind
getAllPs :: (Num a, Fractional a, Eq a) => [a] -> [Int] -> [Polynom a]
-- TODO: Man muss nur das letzte Element in der Liste verändern
getAllPs es ds = [PMS (map (A.second (e*))$ unPMS f) True | f <- getAllMonicPs es ds
                                         , e <- es , e /= 0]

getAllMonicP :: (Num a, Fractional a, Eq a) => [a] -> Int -> [Polynom a]
getAllMonicP es d = getAllMonicPs es [0..d]

getAllMonicPs :: (Num a, Fractional a, Eq a) => [a] -> [Int] -> [Polynom a]
getAllMonicPs es is = map (\x -> PMS x True) $ concat [allMonics i | i <- is]
  where allMonics 0 = [[(0,1)]]
        allMonics i = [[(i,1)]] ++ [(i,1):rs | rs <- ess (i-1)]
        ess i       | i == 0     = [[(0,y)] | y <- swpes]
                    | otherwise = [[(i,y)] | y <- swpes] ++ (ess (i-1)) ++ 
                              [(i,y):ys | y <- swpes, ys <- ess (i-1)]
        swpes       = filter (/=0) es
        {-one         = (\ x -> x / x) $ head [e | e <- es, e /= 0]-}
