module Projekt.Core.Matrix
  ( Matrix (..), unM
  -- getter
  , atM, getNumRowsM, getNumColsM
  -- operations
  , swapRowsM, swapColsM
  , triangular, triangular', pivotAndSwap
  ) where
import Data.List

import Projekt.Core.ShowTex
import Debug.Trace

--------------------------------------------------------------------------------
--  Data Definition

data Matrix a = M [[a]] | Mdiag a

--------------------------------------------------------------------------------
--  Basics

unM :: Matrix a -> [[a]]
unM (M m) = m

isValidM :: Matrix a -> Bool
isValidM (M m) = and [n == head ns | n <- tail ns]
  where ns = map length m

genDiagM :: (Num a) => a -> Int -> Matrix a
genDiagM x n = M [genEyeM' i | i <- [0..(n-1)]]
  where genEyeM' i = [0 | j <- [0..(i-2)]] ++ (x:[0 | j <- [i..(n-1)]])

--------------------------------------------------------------------------------
--  Instanzen

instance Show a => Show (Matrix a) where
  show m = concatMap ((++ "\n") . show') $ unM m
    where show' (x:xs) = (show x ++) $ concatMap ((' ':) . show) xs

instance (ShowTex a,Eq a) => ShowTex (Matrix a) where
  showTex (M [])    = ""
  showTex (M [[]])  = ""
  showTex (Mdiag a) = "[" ++ showTex a ++ "]"
  showTex (M m)     = "\\begin{pmatrix}" ++ showTex' m ++ "\\end{pmatrix}"
    where showTex'         = concatMap ((++ "\\\\") . showTex'')
          showTex'' (x:xs) = (showTex x ++) $ concatMap (('&':) . showTex) xs

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

-- TODO:
addM :: (Num a) => Matrix a -> Matrix a -> Matrix a
addM (Mdiag x) (Mdiag y) = Mdiag (x+y)
addM (Mdiag x) m         = addM m (genDiagM x (getNumRowsM m))
addM (M x)     (M y)     | test      = addM' (length x) (length (head x))
                         | otherwise = error "not the same Dimensions"
  where test      = (length x == length y) && (length (head x) == length (head y))
        addM' n m = M [[x!!i!!j + y!!i!!j | j <- [0..(m-1)]] | i <- [0..(n-1)]]

-- TODO:
multM :: (Num a) => Matrix a -> Matrix a -> Matrix a
multM (Mdiag x) (Mdiag y) = Mdiag (x*y)
multM (Mdiag x) m         = multM m (genDiagM x (getNumRowsM m))
multM (M m)     (M n)     | test      = undefined
                          | otherwise = error "not the same Dimensions"
  where test = length (head m) == length n

negateM :: (Num a) => Matrix a -> Matrix a
negateM (Mdiag x) = Mdiag $ negate x
negateM (M m)     = M $ map (map negate) m

--------------------------------------------------------------------------------
--  Getter

atM :: Matrix a -> Int -> Int -> a
atM m row col = unM m !! row !! col

getNumRowsM :: Matrix a -> Int
getNumRowsM = length . unM

getNumColsM :: Matrix a -> Int
getNumColsM = length . head . unM

--------------------------------------------------------------------------------
--  Funktionen auf Matrizen

-- |Transponiere eine Matrix
transposeM :: Matrix a -> Matrix a
transposeM = M . transpose . unM

-- |Vertausche zwei Zeilen einer Matrix
swapRowsM :: Matrix a -> Int -> Int -> Matrix a
swapRowsM m r1 r2 = M $ swapItems (unM m) r1 r2

-- |Vertausche zwei Spalten einer Matrix
swapColsM :: Matrix a -> Int -> Int -> Matrix a
swapColsM m r1 r2 = M $ map (\x -> swapItems x r1 r2) $ unM m

swapItems :: [a] -> Int -> Int -> [a]
swapItems ls r1 r2 = [get k x | (x,k) <- zip ls [0..]]
    where get k x | k == r1 = ls !! r1
                  | k == r1 = ls !! r2
                  | otherwise = x


triangular :: (Eq a, Fractional a, Show a) => Matrix a -> Matrix a
triangular m  = M $ triangular' 0 $ unM m

triangular' n [] = []
triangular' n m' = row : (triangular' (n+1) rows')
  where
  (row:rows) = pivotAndSwap n m'
  rows'      = map eval rows
  eval rs
        | (rs !! n) == 0 = rs
        | otherwise     = zipWith (-) (map (*c) rs) row
    where c = (row !! n) / (rs !! n)


pivotAndSwap :: (Eq a, Num a) => Int -> [[a]] -> [[a]]
pivotAndSwap n (row:rows) | (row !! n) /= 0 
                          || length rows == 0
                          || all (==True) (map (all (==0)) rows)  = (row:rows)
                        | otherwise       = pivotAndSwap n (rows ++ [row])


