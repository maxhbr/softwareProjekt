module Projekt.Core.Matrix
  ( Matrix (..), genDiagM, fromListsM
  -- getter
  , atM, getNumRowsM, getNumColsM
  -- tests
  , isQuadraticM
  -- operations
  , transposeM
  -- Funktionen
  , swapRowsM, swapColsM, subM
  -- linear algebra
  , detLapM, detM, echelonM
  )
  where
import Data.List
import Data.Array
import Data.Array.IArray (amap)

import Projekt.Core.ShowTex
import Debug.Trace

--------------------------------------------------------------------------------
--  Data Definition

-- Zeile x Spalte
data Matrix a = M {unM :: Array (Int, Int) a} | Mdiag a

--------------------------------------------------------------------------------
--  Basics

fillList ls n m = ls ++ [(idx,0) | idx <- getAllIdxsExcept n m idxs]
  where idxs                      = map fst ls
        getAllIdxsExcept n m idxs = [idx | idx <- [(i,j) | i <- [1..n], j <- [1..m]]
                                         , idx `notElem` idxs]

isQuadraticM :: Matrix a -> Bool
sQuadraticM (Mdiag a) = True
isQuadraticM (M m) = uncurry (==) b
  where b = snd $ bounds m

genDiagM :: Num a => a -> Int -> Matrix a
genDiagM x n = M $ array ((1,1),(n,n)) $ fillList [((i,i),x) | i <- [1..n]] n n

fromListsM :: [[a]] -> Matrix a
fromListsM ess = M $ array ((1,1),(k,l)) [((i,j),ess!!(i-1)!!(j-1)) | i <- [1..k]
                                                                    , j <- [1..l]]
  where k = length ess
        l = length $ head ess

--------------------------------------------------------------------------------
--  Getter

atM :: Matrix a -> Int -> Int -> a
atM (M m) row col = m!(row,col)

getNumRowsM :: Matrix a -> Int
getNumRowsM (M m) = fst $ snd $ bounds m

getNumColsM :: Matrix a -> Int
getNumColsM (M m) = snd $ snd $ bounds m

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
  x + y         = addM x y
  x * y         = multM x y
  fromInteger i = Mdiag (fromInteger i)
  abs _         = error "Prelude.Num.abs: inappropriate abstraction"
  signum _      = error "Prelude.Num.signum: inappropriate abstraction"
  negate        = negateM

addM :: (Num a) => Matrix a -> Matrix a -> Matrix a
addM (Mdiag x) (Mdiag y) = Mdiag (x+y)
addM (Mdiag x) m         = addM m (genDiagM x (getNumRowsM m))
addM m         (Mdiag y) = addM m (genDiagM y (getNumRowsM m))
addM (M x)     (M y)     | test      = M $ array (bounds x)
    [(idx,x!idx + y!idx) | idx <- indices x]
                         | otherwise = error "not the same Dimensions"
  where test      = bounds x == bounds y

negateM :: (Num a) => Matrix a -> Matrix a
negateM (Mdiag x) = Mdiag $ negate x
negateM (M m)     = M $ amap negate m

multM :: (Num a) => Matrix a -> Matrix a -> Matrix a
multM (Mdiag x) (Mdiag y) = Mdiag (x*y)
multM (Mdiag x) m         = multM (genDiagM x (getNumRowsM m)) m
multM  m        (Mdiag x) = multM m (genDiagM x (getNumColsM m))
multM (M m)     (M n)     | k' == l    = M $ array ((1,1),(k,l'))
    [((i,j), sum [m!(i,k) * n!(k,j) | k <- [1..l]]) | i <- [1..k] , j <- [1..l']]
                          | otherwise = error "not the same Dimensions"
  where ((_,_),(k,l))   = bounds m
        ((_,_),(k',l')) = bounds n

--------------------------------------------------------------------------------
--  Funktionen auf Matrizen

-- |Gibt zu einer Matrix eine Untermatrix zurrück
-- Input:
--      (k0,l0) : erste übernommene Spalte und Zeile
--      (k1,l1) : letzte übernommene Spalte und Zeile
--      m       : eingabe Matrix
subM :: Num a => (Int,Int) -> (Int,Int) -> Matrix a -> Matrix a
subM (k0,l0) (k1,l1) (Mdiag x) = subM (k0,l0) (k1,l1) $ genDiagM x $ max k1 l1
subM (k0,l0) (k1,l1) (M m)     = M $ subArr (k0,l0) (k1,l1) m

subArr :: Num a => (Int,Int) -> (Int,Int) -> Array (Int, Int) a
                                                          -> Array (Int, Int) a
subArr (k0,l0) (k1,l1) m =  array ((1,1),(k,l))
    [ ((i-k0+1,j-l0+1) , m!(i,j)) | i <- [k0..k1] , j <- [l0..l1]]
  where (k,l) = (k1-k0+1,l1-l0+1)

swapRowsM :: Num a => Int -> Int -> Matrix a -> Matrix a
swapRowsM _ _ (Mdiag x) = error "Not enougth information given"
swapRowsM k0 k1 (M m)   = M $ swapRowsArr k0 k1 m

swapRowsArr :: Num a => Int -> Int -> Array (Int, Int) a -> Array (Int, Int) a
swapRowsArr k0 k1 m = array ((1,1),(k,l))
    [ ((swp i,j) , m!(i,j)) | i <- [1..k] , j <- [1..l]]
  where (k,l) = snd $ bounds m
        swp i | i == k0    = k1
              | i == k1    = k0
              | otherwise = i

swapColsM :: Num a => Int -> Int -> Matrix a -> Matrix a
swapColsM _ _ (Mdiag x) = error "Not enougth information given"
swapColsM l0 l1 (M m) = M $ swapColsArr l0 l1 m

swapColsArr :: Num a => Int -> Int -> Array (Int, Int) a -> Array (Int, Int) a
swapColsArr l0 l1 m = array ((1,1),(k,l))
    [ ((i,swp j) , m!(i,j)) | i <- [1..k] , j <- [1..l]]
  where (k,l) = snd $ bounds m
        swp j | j == l0    = l1
              | j == l1    = l0
              | otherwise = j

-- |Transponiere eine Matrix
transposeM :: Matrix a -> Matrix a
transposeM (Mdiag a) = Mdiag a
transposeM (M m)     = M $ ixmap (bounds m) (\(x,y) -> (y,x)) m

-- |Berechne die Determinante ohne nutzen von Fractional a
detLapM :: (Eq a, Num a) => Matrix a -> a
detLapM (Mdiag 0) = 0
detLapM (Mdiag 1) = 1
detLapM (Mdiag _) = error "Not enougth information given"
detLapM m | isQuadraticM m = detLapM' $ unM m
          | otherwise      = 0
detLapM' :: (Eq a, Num a) => Array (Int, Int) a -> a
detLapM' m | b == (1,1) = m!(1,1)
           | otherwise =
  sum [(-1)^(i-1) * m!(i,1) * detLapM' (getSubArr i) | i <- [1..fst b]]
    where b           = snd $ bounds m
          getSubArr i = array ((1,1),(fst b-1,snd b-1)) $
            [((i',j'),m!(i',j'+1)) | i' <- [1..(i-1)]
                                   , j' <- [1..(snd b - 1)]]
            ++ [((i',j'),m!(i'+1,j'+1)) | i' <- [i..(fst b - 1)]
                                       , j' <- [1..(snd b - 1)]]

-- |Berechne die Determinante effektiver als detLapM aber braucht Fractional
detM :: (Eq a, Num a, Fractional a) => Matrix a -> a
detM (Mdiag 0) = 0
detM (Mdiag 1) = 1
detM (Mdiag _) = error "Not enougth information given"
detM m         | isQuadraticM m = detArr $ unM m
               | otherwise      = error "Matrix not quadratic"
  where -- |detM auf Array ebene
        detArr :: (Eq a, Num a, Fractional a) => Array (Int, Int) a -> a
        detArr m | k == 1       = m!(1,1)
                 | m!(1,1) == 0 = - detArrPivot m
                 | otherwise = (m!(1,1) *) $ detArr $ subArr (2,2) (k,l) $
                   arrElim m
          where (k,l) = snd $ bounds m

        -- |Sucht ein Pivot Element und vertauscht wenn nötig
        detArrPivot :: (Eq a, Num a, Fractional a) => Array (Int, Int) a -> a
        detArrPivot m | null lst  = 0
                      | otherwise = detArr $ swapRowsArr 1 (minimum lst) m
          where (k,l) = snd $ bounds m
                lst   = [i | i <- [1..k] , m!(i,1) /= 0]


-- |Zieht die erste Zeile passend von allen anderen ab
arrElim :: (Eq a, Num a, Fractional a) => Array (Int, Int) a
                                                  -> Array (Int, Int) a
arrElim m = 
  (m // [ ((1,j),m!(1,j)/m!(1,1)) | j <- [1..l]])
    // [ ((i,j), m!(i,j) - m!(i,1) / m!(1,1) * m!(1,j)) | j <- [1..l],
        i <- [2..k]]
  where (k,l) = snd $ bounds m


echelonM :: (Eq a, Num a, Fractional a) => Matrix a -> Matrix a
echelonM (Mdiag n) = Mdiag n
echelonM m         = M $ echelonM' $ unM m
  where echelonM' :: (Eq a, Num a, Fractional a) =>     
                    Array (Int,Int) a -> Array (Int,Int) a
        echelonM' m | k == 1       = arrElim m
                    | m!(1,1) == 0 = echelonM' $ swapRowsArr 1 (minimum lst) m
                    | otherwise   = m' // shifted
          where (k,l) = snd $ bounds m
                lst   = [i | i <- [1..k], m!(i,1) /= 0]
                m' = arrElim m
                shifted = map (\((i,j),x) -> ((i+1,j+1),x)) $ assocs m''
                m''     = echelonM' $ subArr (2,2) (k,l) $ m'
