--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Core.Polynomials
-- Note        : Allgemeine implementierung von Polynomen in einer Variablen
--
--
--
--------------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module GalFld.Core.Polynomials
  ( Polynom, pList, pTup, pTupUnsave, pKonst, p2Tup, p2List, cleanP
  , nullP, isNullP, isNullP'
  -- getter
  , getDegrees, getLcP
  -- Operationen auf Polynomen
  , degP, uDegP
  -- unär
  , moniP, moniLcP, deriveP, reciprocP, reciprocP2, multMonomP
  -- binär
  , divP, (@/), modByP, ggTP, eekP, divPInv
  -- weiteres
  , evalP, hasNs
  , addPM, multPM
  , getAllP, getAllPs
  , getAllMonicP, getAllMonicPs
  , multPK, multPMKaratsuba
  ) where
import Data.List
import qualified Control.Arrow as A
import Data.Maybe
import Data.Binary
import Data.Ord
import Control.DeepSeq

import GalFld.Core.ShowTex


--------------------------------------------------------------------------------
--  Data Definition

-- |Polynome sind Listen von Monomen, welche durch Paare (In,a)
-- dargestellt werden. An erster Stelle steht der Grad, an zweiter der
-- Koeffizient.
data Polynom a = PMS { unPMS :: [(Int,a)], clean :: Bool } deriving ()

-- |Das Nullpolynom
nullP = PMS [] True

-- |Erzeugt ein Polynom aus einer Liste von Koeffizienten
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
pKonst :: (Eq a, Num a) => a -> Polynom a
pKonst x | x == 0     = nullP
         | otherwise = PMS [(0,x)] True

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
tuple2List :: (Num a) => [(Int,a)] -> [a]
tuple2List [] = []
tuple2List ((i,m):ms) = zipSum ([0 | j <- [1..i]] ++ [m]) $ tuple2List ms

-- |Wandelt eine Liste von Koeffizienten in eine Liste von Monomen.
--  Diese ist bereits dem Grade nach absteigend sortiert.
list2TupleSave :: (Eq a, Num a) => [a] -> [(Int,a)]
list2TupleSave ms = list2Tuple' ms 0
  where list2Tuple' [] n                 = []
        list2Tuple' (m:ms) n | m == 0     = list2Tuple' ms (n+1)
                             | otherwise = list2Tuple' ms (n+1) ++ [(n,m)]


instance (Eq a, Num a) => Eq (Polynom a) where
  f == g = eqP f g

{-# INLINE eqP #-}
eqP :: (Eq a, Num a) => Polynom a -> Polynom a -> Bool
eqP (PMS ms True) (PMS ns True)  = eqP' ms ns
  where eqP' [] ns = isNullP' ns
        eqP' ms [] = isNullP' ms
        eqP' ((i,m):ms) ((j,n):ns) =  i==j && m==n && eqP' ms ns
eqP f g  = eqP (cleanP f) (cleanP g)

{-# INLINE isNullP #-}
isNullP (PMS ms _) = isNullP' ms
isNullP' []        = True
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
  {-# INLINE (-) #-}
  f@(PMS _ _) - g@(PMS _ _) = PMS hs True
    where hs = subtrPM (unPMS $ cleanP f) (unPMS $ cleanP g)
  {-# INLINE (*) #-}
  f@(PMS _ _) * g@(PMS _ _) = PMS hs True
    where hs = multPM (unPMS $ cleanP f) (unPMS $ cleanP g)
  fromInteger i     = PMS [(0,fromInteger i)] True
  abs _             = error "Prelude.Num.abs: inappropriate abstraction"
  signum _          = error "Prelude.Num.signum: inappropriate abstraction"
  negate (PMS ms b) = PMS ((map . A.second) negate ms) b

{-# INLINE addPM #-}
-- |Addiert Polynome in Monomdarstellung, d.h
--  [(Int,a)] wobei die Liste in Int ABSTEIGEND sortiert ist
addPM :: (Eq a,Num a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)]
addPM [] gs          = gs
addPM fs []          = fs
addPM ff@((i,f):fs) gg@((j,g):gs)
  | i==j && c/=0  = (i,c) : addPM fs gs
  | i==j && c==0  = addPM fs gs
  | i<j         = (j,g) : addPM ff gs
  | i>j         = (i,f) : addPM fs gg
   where !c = f+g

{-# INLINE subtrPM #-}
-- |Subtrahiert Polynome in Monomdarstellung, d.h
--  [(Int,a)] wobei die Liste in Int ABSTEIGEND sortiert ist
subtrPM :: (Eq a,Num a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)]
subtrPM [] gs          = map (A.second negate) gs
subtrPM fs []          = fs
subtrPM ff@((i,f):fs) gg@((j,g):gs)
  | i==j && c/=0  = (i,c) : subtrPM fs gs
  | i==j && c==0  = subtrPM fs gs
  | i<j         = (j,negate g) : subtrPM ff gs
  | i>j         = (i,f) : subtrPM fs gg
   where !c = f-g


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
multPM f [] = []
multPM [] f = []
multPM ms ns = foldr1 addPM summanden
  where  summanden = [multPM' i m ns | (i,m) <- ms]

{-# INLINE multPM' #-}
multPM' i m []                     = []
multPM' i m ((j,n):ns) | c == 0     = multPM' i m ns
                       | otherwise = (k,c) : multPM' i m ns
  where !c = n*m
        !k = i+j

-------------------------------------------------------------------------------
-- Karatsuba Multiplikation

{-# INLINE multPK #-}
multPK :: (Show a, Num a, Eq a) => Polynom a -> Polynom a -> Polynom a
multPK f g = PMS h True
  where h = multPMKaratsuba ((unPMS.cleanP) f) ((unPMS.cleanP) g)

{-# INLINE multPMKaratsuba #-}
multPMKaratsuba :: (Show a, Num a, Eq a) => [(Int,a)] -> [(Int,a)] -> [(Int,a)]
multPMKaratsuba f g  = multPMK' n f g
  where n  = next2Pot (max df dg) `quot` 2
        df = if null f then 0 else fst (head f) + 1
        dg = if null g then 0 else fst (head g)+ 1

-- Der eigentliche Karatsuba
multPMK' :: (Show a, Num a, Eq a) => Int -> [(Int,a)] -> [(Int,a)] -> [(Int,a)]
multPMK' _ _ [] = []
multPMK' _ [] _ = []
multPMK' _ [(i,x)] g = map ((+) i A.*** (*) x) g
multPMK' _ f [(i,x)] = map ((+) i A.*** (*) x) f
multPMK' 1 [(i1,x1),(i2,x2)] [(j1,y1),(j2,y2)]
      = [(2,p1), (1,p3-p1-p2), (0,p2)]
  where !p1 = x1*y1
        !p2 = x2*y2
        !p3 = (x1+x2)*(y1+y2)
multPMK' n f g = addPM e1 $ addPM e2 e3
  where  -- High und Low Parts
        {-# INLINE fH' #-}
        fH' = takeWhile (\(i,_) -> i>=n) f
        {-# INLINE fH #-}
        fH = map (A.first (\i -> i-n)) fH'
        {-# INLINE fL #-}
        fL = f \\ fH'
        {-# INLINE gH' #-}
        gH' = takeWhile (\(i,_) -> i>=n) g
        {-# INLINE gH #-}
        gH = map (A.first (\i -> i-n)) gH'
        {-# INLINE gL #-}
        gL = g \\ gH'
        -- Rekursiver Karatsuba
        {-# INLINE p1 #-}
        p1 = multPMK' (n `quot` 2) fH gH
        {-# INLINE p2 #-}
        p2 = multPMK' (n `quot` 2) fL gL
        {-# INLINE p3 #-}
        p3 = multPMK' (n `quot` 2) (addPM fH fL) (addPM gH gL)
        {-# INLINE e1 #-}
        e1 = map (A.first (+(2*n))) p1
        {-# INLINE e2 #-}
        e2 = map (A.first (+n)) $ subtrPM p3 (addPM p1 p2)
        {-# INLINE e3 #-}
        e3 = p2


{-# INLINE next2Pot #-}
next2Pot :: Int -> Int
next2Pot l = next2Pot' 1
  where next2Pot' n | n >= l     = n
                    | otherwise = next2Pot' $! 2*n



{-# INLINE multMonomP #-}
-- |Multipliziert f mit x^i
multMonomP :: (Eq a, Num a) => Int -> Polynom a -> Polynom a
multMonomP i (PMS ms b)   = PMS (map (A.first (+i)) ms) b

{-# INLINE multKonstP #-}
multKonstP :: (Eq a, Num a) => a -> Polynom a -> Polynom a
multKonstP a f  = PMS (map (A.second (*a)) ms) True
  where ms = unPMS $ cleanP f



{-# INLINE getLcP #-}
getLcP :: (Num a, Eq a) => Polynom a -> a
getLcP (PMS [] _)    =  0
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
   put (PMS x _) = put x
   get       = do x <- get
                  return $ PMS x False

instance (Eq a, Num a, NFData a) => NFData (Polynom a) where
  rnf = rnf . map fst . p2Tup

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

-- |Normiert f und gibt gleichzeitig das Inverse des Leitkoeffizienten zurück
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
        ms = map (A.first (k -)) $ unPMS f

-- |divP mit Horner Schema
--  siehe http://en.wikipedia.org/wiki/Synthetic_division
divP :: (Show a, Eq a, Fractional a) =>
                              Polynom a -> Polynom a -> (Polynom a,Polynom a)
divP = divPHorner

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

{-# INLINE divPHornerM' #-}
-- |Horner für absteigend sortierte [(Int,a)] Paare
divPHornerM' _  [] _ _ = []
divPHornerM' divs ff@((i,f):fs) lc n
  | n > fst (head ff)  = ff
  | otherwise            = (i,fbar) : divPHornerM' divs hs lc n
  where fbar = f/lc
        {-# INLINE hs #-}
        hs   = addPM fs $! js
        {-# INLINE js #-}
        js   = map ( (+) (i-n) A.*** (*) fbar) divs

{-# INLINE divP' #-}
divP' :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
divP' a b = fst $ divP a b

{-# INLINE (@/) #-}
(@/) :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
(@/) = divP'

{-# INLINE modByP #-}
-- |Nimmt ein Polynom und rechnet modulo ein anderes Polynom.
--  Also Division mit Rest und Rückgabe des Rests.
modByP :: (Show a, Eq a, Fractional a) => Polynom a -> Polynom a -> Polynom a
modByP f p = snd $ divP f p

-- |Hensel inverse lift
--  Input: Polynom h mit h(0) = 1
--         Int k
--  Output: h^(-1) mod x^k
--invModMonom :: (Show a, Num a, Eq a, Fractional a) => Polynom a -> Int -> Polynom a
--invModMonom h k  | isNullP h  = nullP
--               | otherwise  = invModMonom' (pKonst 1) 1 1 [] (unPMS h)
--  where invModMonom' !a !l !lold !h0 !h1
--          | l >= k     = modMonomP k a
--          | otherwise = invModMonom' (a+a') l' l h0' h1'
--          where b = negate $ multPShortDown l a $!
--                            multPShortDown l a (PMS h1' True) + PMS c True
--                a' = multMonomP l b
--                l' = 2*l
--                h1' = map (\(i,m) -> (i-lold,m)) $ takeWhile (\(i,_) -> i>=lold) h1
--                h0'' = filter (\(i,_) -> i<lold) h1
--                h0''' | lold==1 = h0''
--                      | otherwise = map (\(i,m) -> (i+lold,m)) h0''
--                h0' = h0''' ++ h0
--                c   = map (\(i,m) -> (i-l,m)) $ multPM_ShortUp l (unPMS a) h0'

invModMonom :: (Show a, Num a, Eq a, Fractional a) => Polynom a -> Int -> Polynom a
invModMonom h k  | isNullP h  = nullP
               | otherwise  = PMS (invModMonom' [(0,1)] 1) True
  where hs = unPMS $ cleanP h
        invModMonom' !a !l
          | l >= k     =  a
          | otherwise = invModMonom' b lnew
          where -- g_i+1 = (2*g_i - h*g_i^2) mod x^(2^i)
                b = map (A.second negate) a' ++ a
                -- a' = h*g_i^2
                a' = multPMInter lnew l hs $ multPMInter lnew 0 a a
                -- nächster Schritt
                lnew = 2*l

{-# INLINE multPInter #-}
-- |Multipliziert f mit g, wobei nur Terme mit x^l für 
--  l > lLow und l < lHigh betrachtet werden
multPInter :: (Show a, Eq a, Num a) => Int -> Int -> Polynom a -> Polynom a -> Polynom a
multPInter _ _ (PMS [] _) _ = nullP
multPInter _ _ _ (PMS [] _) = nullP
multPInter lHigh lLow f g
      = PMS (multPMInter lHigh lLow ((unPMS.cleanP) f) ((unPMS.cleanP) g)) True

{-# INLINE multPMInter #-}
-- |Multipliziert f mit g, wobei nur Terme mit x^l für 
--  l > lLow und l < lHigh betrachtet werden
multPMInter :: (Show a, Eq a, Num a) => Int -> Int ->
                                [(Int,a)] -> [(Int,a)] -> [(Int,a)]
multPMInter _ _ f [] = []
multPMInter _ _ [] f = []
multPMInter lHigh lLow ms ns = foldr1 addPM summanden
  where summanden = [multPMInter'  i m ns | (i,m) <- ms]
        {-# INLINE multPMInter' #-}
        multPMInter' i m [] = []
        multPMInter' i m ((j,n):ns) 
          | k < lLow || k >= lHigh || c == 0 = multPMInter' i m ns
          | otherwise                      = (k,c) : multPMInter' i m ns
          where !c = n*m
                !k = i+j


{-# INLINE modMonomP #-}
modMonomP :: (Eq a, Num a) => Int -> Polynom a -> Polynom a
modMonomP _ (PMS [] _)    = nullP
modMonomP l (PMS ms True) = PMS (dropWhile (\(i,_) -> i>=l) ms) True
modMonomP l f             = modMonomP l $ cleanP f


divPInv :: (Show a, Eq a, Fractional a) =>
              Polynom a -> Polynom a -> (Polynom a, Polynom a)
divPInv a b
    | isNullP a = (nullP, nullP)
    | a == b     = (pKonst 1,nullP)
    | l <= 0     = (nullP,a)
    | otherwise = (q',r)
  where n  = uDegP a
        m  = uDegP b
        l  = n-m+1
        (lc,b') = moniLcP b
        f  = reciprocP2 m b'
        g  = invModMonom f l
        q  = multPInter l 0 g $ reciprocP2 n a
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
-- |Nimmt einen Wert und ein Polynom und wertet es dort aus.
--  Mittels Horner Schema
evalP :: (Eq a, Num a) => a -> Polynom a -> a
evalP x f = evalP' x (unPMS $ cleanP f)
evalP' :: (Num a) => a -> [(Int,a)] -> a
evalP' x []   = 0
evalP' x fs   = snd $ foldl' (\(i,z) (j,y) -> (j,z*x^(i-j)+y)) (head fs) (tail fs)

hasNs :: (Eq a, Fractional a) => Polynom a -> [a] -> Bool
hasNs f es = not (null [f | e <- es, evalP e f == 0])

hasNs' f es = [evalP e f | e <- es]
--------------------------------------------------------------------------------
--  liste alle möglichen Polynome auf

-- |Nimmt eine Liste und Grad und erzeugt daraus alle Polynome bis zu diesem
-- Grad.
-- Das Nullpolynom (P[]) ist NICHT enthalten.
getAllP :: (Num a, Fractional a, Eq a) => [a] -> Int -> [Polynom a]
getAllP es d = [PMS (map (A.second (e*)) $ unPMS f) True | f <- getAllMonicP es d
                                       , e <- es , e /= 0]

-- |Nimmt eine Liste und eine Liste von Grade und erzeugt daraus alle
-- Polynome, deren Grade in der Liste enthalten sind.
getAllPs :: (Num a, Fractional a, Eq a) => [a] -> [Int] -> [Polynom a]
getAllPs es ds = [PMS (map (A.second (e*))$ unPMS f) True 
                   | f <- getAllMonicPs es ds , e <- es , e /= 0]

getAllMonicP :: (Num a, Fractional a, Eq a) => [a] -> Int -> [Polynom a]
getAllMonicP es d = getAllMonicPs es [0..d]

getAllMonicPs :: (Num a, Fractional a, Eq a) => [a] -> [Int] -> [Polynom a]
getAllMonicPs es is = map (`PMS` True) $ concat [allMonics i | i <- is]
  where allMonics 0 = [[(0,1)]]
        allMonics i = [(i,1)] : [(i,1):rs | rs <- ess (i-1)]
        ess i       | i == 0     = [[(0,y)] | y <- swpes]
                    | otherwise = [[(i,y)] | y <- swpes] ++ ess (i-1) ++
                              [(i,y):ys | y <- swpes, ys <- ess (i-1)]
        swpes       = filter (/= 0) es
