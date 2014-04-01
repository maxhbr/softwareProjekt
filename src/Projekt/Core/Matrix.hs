module Projekt.Core.Matrix 
  where
import Data.List

-- 
data Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
 show m = concat $ map ((++ ['\n']) . concat . map( (' ':) . show) ) $ unM m

unM :: Matrix a -> [[a]]
unM (Matrix m) = m


atM :: Matrix a -> Int -> Int -> a
atM m row col = (unM m) !! row !! col


swapRowsM :: Matrix a -> Int -> Int -> Matrix a
swapRowsM m r1 r2 = Matrix $ swapItems (unM m) r1 r2

swapColsM :: Matrix a -> Int -> Int -> Matrix a
swapColsM m r1 r2 = Matrix $ map (\x -> swapItems x r1 r2) $ unM m


swapItems :: [a] -> Int -> Int -> [a]
swapItems ls r1 r2 = [get k x | (x,k) <- zip ls [0..]]
    where get k x | k == r1 = ls !! r1
                  | k == r1 = ls !! r2
                  | otherwise = x

