{-# LANGUAGE BangPatterns #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Core.Matrix
-- Note        :
--
--
--
--------------------------------------------------------------------------------
module GalFld.Core.Matrix
  ( Matrix (..), genDiagM, fromListsM, toListsM
  -- getter
  , atM, getNumRowsM, getNumColsM, getRowM, getColM, boundsM
  -- Tests
  , isQuadraticM
  -- Operationen
  , transposeM
  -- Funktionen
  , (<|>), (<->)
  , swapRowsM, swapColsM, subM
  -- lineare Algebra
  , detLapM, detM, echelonM, kernelM
  -- Weiteres
  , getAllM
  )
  where

import Debug.Trace

import Data.List
import Data.Array
import Data.Array.IArray (amap)
import Data.Binary
import Control.Monad

import GalFld.Core.ShowTex

--------------------------------------------------------------------------------
--  Data Definition


-- Eine Matrix ist im inneren als ein zwei dimensionales Array dargestellt,
-- wobei die erste Stelle die Zeile und dei zweite die Spalte darstellt
data Matrix a = M {unM :: Array (Int, Int) a} | Mdiag a

--------------------------------------------------------------------------------
--  Basics

{-# INLINE genDiagM #-}
-- |Erzeugt ein Vielfaches der Einheitsmatrix
genDiagM :: Num a => a -> Int -> Matrix a
genDiagM x n = M $ array ((1,1),(n,n)) $ fillList [((i,i),x) | i <- [1..n]] n n
  where fillList ls n m = ls ++ [(idx,0) | idx <- getAllIdxsExcept n m idxs]
          where idxs                      = map fst ls
                getAllIdxsExcept n m idxs = [idx | idx <- [(i,j) | i <- [1..n]
                                                                , j <- [1..m]]
                                                 , idx `notElem` idxs]

{-# INLINE fromListsM #-}
-- |Erzeugt eine Matrix aus einer Liste von Listen von Einträgen
fromListsM :: [[a]] -> Matrix a
fromListsM []  = error "GalFld.Core.Matrix.fromListsM: empty lists"
fromListsM [[]]  = error "GalFld.Core.Matrix.fromListsM: empty lists"
fromListsM ess = M $ array ((1,1),(k,l))
                           [((i,j),ess!!(i-1)!!(j-1)) | i <- [1..k]
                                                      , j <- [1..l]]
  where k = length ess
        l = length $ head ess


{-# INLINE toListsM #-}
-- |Erzeugt aus einer Matrix eine Liste von Listen der Einträge. Ist invers zu
-- fromListsM
toListsM :: Matrix a -> [[a]]
toListsM (M m) = [[m!(i,j) | j <- [1..l]] | i <- [1..k]]
  where (k,l) = snd $ bounds m

--------------------------------------------------------------------------------
--  Getter

-- |Gibt zu einer Matrix den Wert an der Position (row,col) zurrück
{-# INLINE atM #-}
atM :: Matrix a -> Int -> Int -> a
atM (M m) row col = m!(row,col)

-- |Gibt zu einer Matrix die Anzahl der Zeilen zurrück
{-# INLINE getNumRowsM #-}
getNumRowsM :: Matrix a -> Int
getNumRowsM (M m) = fst $ snd $ bounds m

-- |Gibt zu einer Matrix die Anzahl der Spalten zurrück
{-# INLINE getNumColsM #-}
getNumColsM :: Matrix a -> Int
getNumColsM (M m) = snd $ snd $ bounds m

-- |Gibt zu einer Matrix die i-te Zeile zurrück
{-# INLINE getRowM #-}
getRowM :: Matrix a -> Int -> [a]
getRowM (M m) i = [m!(i,j) | j <- [1..l]]
  where (k,l) = snd $ bounds m

-- |Gibt zu einer Matrix die i-te Spalte zurrück
{-# INLINE getColM #-}
getColM :: Matrix a -> Int -> [a]
getColM (M m) i = [m!(j,i) | j <- [1..k]]
  where (k,l) = snd $ bounds m

-- |Gibt zu einer Matrix die Grenzen zurrück
-- Das Ergebnis hat die Form ((1,k),(1,l))
{-# INLINE boundsM #-}
boundsM :: Matrix a -> (Int,Int)
boundsM (M m) = snd $ bounds m

{-# INLINE isQuadraticM #-}
isQuadraticM :: Matrix a -> Bool
isQuadraticM (Mdiag a) = True
isQuadraticM (M m) = uncurry (==) $ snd $ bounds m

--------------------------------------------------------------------------------
--  Instanzen

instance Show a => Show (Matrix a) where
  show (Mdiag a) = "diag(" ++ show a ++ "…" ++ show a ++ ")"
  show (M m)     = unlines [concatMap (++ " ") [show (m!(i,j))
                                                | j <- [1..(snd b)]]
                             | i <- [1..(fst b)]]
    where b = snd $ bounds m

instance (ShowTex a,Eq a) => ShowTex (Matrix a) where
  showTex (Mdiag a) = "[" ++ showTex a ++ "]"
  showTex (M m)     = "\\begin{pmatrix}" ++ showTex' ++ "\\end{pmatrix}"
    where (k,l)    = snd $ bounds m
          showTex' = concatMap (++ "\\\\")
            [concatMap ('&':) [showTex (m!(i,j)) | j <- [1..l]] | i <- [1..k]]

instance (Eq a, Num a) => Eq (Matrix a) where
  Mdiag x == m = genDiagM x (getNumRowsM m) == m
  m == Mdiag x = m == genDiagM x (getNumRowsM m)
  M a == M b   = a == b

instance (Num a, Eq a) => Num (Matrix a) where
  {-# INLINE (+) #-}
  x + y         = addM x y
  {-# INLINE (*) #-}
  x * y         = multM x y
  {-# INLINE fromInteger #-}
  fromInteger i = Mdiag (fromInteger i)
  {-# INLINE abs #-}
  abs           = absM
    where {-# INLINE absM #-}
          absM :: (Num a) => Matrix a -> Matrix a
          absM (Mdiag x) = Mdiag $ abs x
          absM (M m)     = M $ amap abs m
  signum _      = error "Prelude.Num.signum: inappropriate abstraction"
  {-# INLINE negate #-}
  negate        = negateM
    where {-# INLINE negateM #-}
          negateM :: (Num a) => Matrix a -> Matrix a
          negateM (Mdiag x) = Mdiag $ negate x
          negateM (M m)     = M $ amap negate m

{-# INLINE addM #-}
addM :: (Num a) => Matrix a -> Matrix a -> Matrix a
addM (Mdiag x) (Mdiag y) = Mdiag (x+y)
addM (Mdiag x) m         = addM m (genDiagM x (getNumRowsM m))
addM m         (Mdiag y) = addM m (genDiagM y (getNumRowsM m))
addM (M x)     (M y)     | boundTest = M $ array (bounds x)
  [(idx,x!idx + y!idx) | idx <- indices x]
                         | otherwise =
  error "GalFld.Core.Matrix.addM: not the same Dimensions"
    where boundTest = bounds x == bounds y

{-# INLINE multM #-}
multM :: (Num a) => Matrix a -> Matrix a -> Matrix a
multM (Mdiag x) (Mdiag y) = Mdiag (x*y)
multM (Mdiag x) m         = multM (genDiagM x (getNumRowsM m)) m
multM  m        (Mdiag x) = multM m (genDiagM x (getNumColsM m))
multM (M m)     (M n)     | k' == l    = M $ array ((1,1),(k,l'))
  [((i,j), sum [m!(i,k) * n!(k,j) | k <- [1..l]]) | i <- [1..k] , j <- [1..l']]
                          | otherwise =
  error "GalFld.Core.Matrix.multM: not the same Dimensions"
    where ((_,_),(k,l))   = bounds m
          ((_,_),(k',l')) = bounds n

instance (Num a, Binary a) => Binary (Matrix a) where
  put (Mdiag m) = do put (0 :: Word8)
                     put m
  put (M x)     = do put (1 :: Word8)
                     put x

  get = do t <- get :: Get Word8
           case t of
                0 -> liftM Mdiag get
                1 -> liftM M get

--------------------------------------------------------------------------------
--  Grundlegende Operationen

{-# INLINE (<|>) #-}
-- |Horizontales Aneinanderfügen von Matrizen
(<|>) :: Matrix a -> Matrix a -> Matrix a
(<|>) (M m1) (M m2) =  M $ array ((1,1),(k1,l1+l2)) $ assocs m1 ++
                             assocs (ixmap ((1,l1+1),(k2,l1+l2))
                                    (\(i,j) -> (i,j-l1)) m2)
  where (k1,l1) = snd $ bounds m1
        (k2,l2) = snd $ bounds m2

{-# INLINE (<->) #-}
-- |Vertikales Aneinanderfügen von Matrizen
(<->) :: Matrix a -> Matrix a -> Matrix a
(<->) (M m1) (M m2) =  M $ array ((1,1),(k1+k2,l1)) $ assocs m1 ++
                             assocs (ixmap ((k1+1,1),(k1+k2,l1))
                                    (\(i,j) -> (i-k1,j)) m2)
  where (k1,l1) = snd $ bounds m1
        (k2,l2) = snd $ bounds m2

--------------------------------------------------------------------------------
--  Funktionen auf Matrizen

{-# INLINE subM #-}
-- |Gibt zu einer Matrix eine Untermatrix zurrück
-- Input:
--      (k0,l0) : erste übernommene Spalte und Zeile
--      (k1,l1) : letzte übernommene Spalte und Zeile
--      m       : Eingabematrix
subM :: Num a => (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
subM (k0,l0) (k1,l1) (Mdiag x) = subM (k0,l0) (k1,l1) $ genDiagM x $ max k1 l1
subM (k0,l0) (k1,l1) (M m)     = M $ subArr (k0,l0) (k1,l1) m


{-# INLINE subArr #-}
-- |Gibt zu einer Matrix eine Untermatrix zurrück
subArr :: Num a => (Int,Int) -> (Int,Int) -> Array (Int, Int) a
                                                          -> Array (Int, Int) a
subArr (k0,l0) (k1,l1) m =  array ((1,1),(k,l))
    [ ((i-k0+1,j-l0+1) , m!(i,j)) | i <- [k0..k1] , j <- [l0..l1]]
  where !(k,l) = (k1-k0+1,l1-l0+1)

{-# INLINE swapRowsM #-}
-- |Vertauscht zwei Zeilen in einer Matrix
swapRowsM :: Num a => Int -> Int -> Matrix a -> Matrix a
swapRowsM _ _ (Mdiag x) =
  error "GalFld.Core.Matrix.swapRowsM: Not enougth information given"
swapRowsM k0 k1 (M m)   = M $ swapRowsArr k0 k1 m

{-# INLINE swapRowsArr #-}
-- |Vertauscht zwei Zeilen in einem Array, das zu einer Matrix gehört
swapRowsArr :: Num a => Int -> Int -> Array (Int, Int) a -> Array (Int, Int) a
swapRowsArr k0 k1 m = array ((1,1),(k,l))
    [ ((swp i,j) , m!(i,j)) | i <- [1..k] , j <- [1..l]]
  where (k,l) = snd $ bounds m
        swp i | i == k0    = k1
              | i == k1    = k0
              | otherwise = i

{-# INLINE swapColsM #-}
-- |Vertauscht zwei Spalten in einer Matrix
swapColsM :: Num a => Int -> Int -> Matrix a -> Matrix a
swapColsM _ _ (Mdiag x) =
  error "GalFld.Core.Matrix.swapColsM: Not enougth information given"
swapColsM l0 l1 (M m) = M $ swapColsArr l0 l1 m

{-# INLINE swapColsArr #-}
-- |Vertauscht zwei Spalten in einem Array, das zu einer Matrix gehört
swapColsArr :: Num a => Int -> Int -> Array (Int, Int) a -> Array (Int, Int) a
swapColsArr l0 l1 m = array ((1,1),(k,l))
    [ ((i,swp j) , m!(i,j)) | i <- [1..k] , j <- [1..l]]
  where (k,l) = snd $ bounds m
        swp j | j == l0    = l1
              | j == l1    = l0
              | otherwise = j

{-# INLINE transposeM #-}
-- |Transponieren einer Matrix
transposeM :: Matrix a -> Matrix a
transposeM (Mdiag a) = Mdiag a
transposeM (M m)     = M $ ixmap ((1,1),(l,k)) (\(x,y) -> (y,x)) m
  where !(k,l) = snd $ bounds m

{-# INLINE detLapM #-}
-- |Berechne die Determinante ohne Nutzen von Fractional a
detLapM :: (Eq a, Num a) => Matrix a -> a
detLapM (Mdiag 0) = 0
detLapM (Mdiag 1) = 1
detLapM (Mdiag _) =
  error "GalFld.Core.Matrix.detLapM: Not enougth information given"
detLapM m | isQuadraticM m = detLapM' $ unM m
          | otherwise      = 0
{-# INLINE detLapM' #-}
detLapM' :: (Eq a, Num a) => Array (Int, Int) a -> a
detLapM' m | b == (1,1) = m!(1,1)
           | otherwise =
  sum [(-1)^(i-1) * m!(i,1) * detLapM' (getSubArr i) | i <- [1..fst b]]
    where !b           = snd $ bounds m
          {-# INLINE getSubArr #-}
          getSubArr i = array ((1,1),(fst b-1,snd b-1)) $
            [((i',j'),m!(i',j'+1)) | i' <- [1..(i-1)]
                                   , j' <- [1..(snd b - 1)]]
            ++ [((i',j'),m!(i'+1,j'+1)) | i' <- [i..(fst b - 1)]
                                       , j' <- [1..(snd b - 1)]]

{-# INLINE detM #-}
-- |Berechne die Determinante effektiver als detLapM aber braucht Fractional
detM :: (Eq a, Num a, Fractional a) => Matrix a -> a
detM (Mdiag 0) = 0
detM (Mdiag 1) = 1
detM (Mdiag _) =
  error "GalFld.Core.Matrix.detM: Not enougth information given"
detM m         | isQuadraticM m = detArr $ unM m
               | otherwise      =
  error "GalFld.Core.Matrix.detM: Matrix not quadratic"
    where {-# INLINE detArr #-}
          -- |detM auf Array ebene
          detArr :: (Eq a, Num a, Fractional a) => Array (Int, Int) a -> a
          detArr m | k == 1       = m!(1,1)
                   | m!(1,1) == 0 = - detArrPivot m
                   | otherwise   = (m!(1,1) *) $ detArr $ subArr (2,2) (k,l) $
                                   arrElim m
            where !(k,l) = snd $ bounds m

          {-# INLINE detArrPivot #-}
          -- |Sucht ein Pivot Element und vertauscht wenn nötig
          detArrPivot :: (Eq a, Num a, Fractional a) => Array (Int, Int) a -> a
          detArrPivot m | null lst  = 0
                        | otherwise = detArr $ swapRowsArr 1 (minimum lst) m
            where !(k,l) = snd $ bounds m
                  !lst   = [i | i <- [1..k] , m!(i,1) /= 0]


{-# INLINE arrElim #-}
-- |Zieht die erste Zeile passend von allen anderen ab, eliminiert also in
-- jeder außer der ersten Zeile den ersten Eintrag der Zeile
arrElim :: (Eq a, Num a, Fractional a) => Array (Int, Int) a
                                                  -> Array (Int, Int) a
arrElim m | m!(1,1) == 0 = m
          | otherwise   =
  (m // [ ((1,j),m!(1,j)/m!(1,1)) | j <- [1..l]])
    // [ ((i,j), m!(i,j) - m!(i,1) / m!(1,1) * m!(1,j)) | j <- [1..l],
        i <- [2..k]]
  where !(k,l) = snd $ bounds m


-- |Berechnet die Zeilenstufenform einer Matrix
{-# INLINE echelonM #-}
echelonM :: (Show a, Eq a, Num a, Fractional a) => Matrix a -> Matrix a
echelonM (Mdiag n) = Mdiag n
echelonM (M m)     = M $ echelonM' m
  where echelonM' :: (Show a, Eq a, Num a, Fractional a) =>
                    Array (Int,Int) a -> Array (Int,Int) a
        echelonM' m | k == 1       = arrElim m
                    | l == 1       = arrElim m
                    | hasPivot    = echelonM' $ swapRowsArr 1 (minimum lst) m
                    | noPivot     = echelonM'_noPivot m
                    | otherwise   = echelonM'_Pivot m
          where !(k,l)    = snd $ bounds m
                !lst      = [i | i <- [1..k], m!(i,1) /= 0]
                !hasPivot = m!(1,1) == 0 && not (null lst)
                !noPivot  = m!(1,1) == 0 && null lst

                {-# INLINE echelonM'_Pivot #-}
                echelonM'_Pivot m = m' // shifted
                  where !m' = arrElim m
                        !shifted = map (\((i,j),x) -> ((i+1,j+1),x)) $ assocs m''
                        !m''     = echelonM' $ subArr (2,2) (k,l) m'

                {-# INLINE echelonM'_noPivot #-}
                echelonM'_noPivot m = m // shifted
                  where !m' = echelonM' $ subArr (1,2) (k,l) m
                        !shifted = map (\((i,j),x) -> ((i,j+1),x)) $ assocs m'

-- |Berechnet den Kern einer Matrix, d.h.
--  kernelM gibt eine Matrix zurück, deren Spalten eine Basis des
--  des Kerns sind
{-# INLINE kernelM #-}
kernelM :: (Show a, Eq a, Num a, Fractional a) => Matrix a -> Matrix a
kernelM (Mdiag m) = error "GalFld.Core.Matrix.kernelM: No kernel here"
kernelM m     = M $ array ((1,1), (k,lzs))
                  [ ((i,j),b!(i,zs!!(j-1))) | i <- [1..k], j <- [1..lzs]]
  where !(k,l) = snd $ bounds $ unM m
        !mfull = transposeM $ echelonM $
                transposeM $ m <-> genDiagM 1 k
        !a     = subArr (1,1) (k,l) $ unM mfull
        !b     = subArr (k+1,1) (k+k,l) $ unM mfull
        !zs    = [j | j <- [1..l], and [a!(i',j) == 0 | i' <- [j..k]]]
        !lzs   = length zs

--------------------------------------------------------------------------------
--  Weiteres

{-# INLINE getAllM #-}
-- |Gibt eine Liste aller Matrizen, welche Einträge aus einer Liste besitzen
-- und eine gewisse größe haben, zurrück
getAllM :: [a] -> (Int,Int) -> [Matrix a]
getAllM cs (k,l) = map fromListsM $ rowMs k
  where lines = lines' l
        lines' n | n == 1     = [[y] | y <- cs]
                 | otherwise = [y:ys | y <- cs, ys <- lines' (n-1) ]
        rowMs n  | n == 1     = [[y] | y <- lines]
                 | otherwise = [y:ys | y <- lines, ys <- rowMs (n-1) ]
