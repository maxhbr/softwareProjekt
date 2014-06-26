module GalFld.Core.NumberTheory (
  möbFkt
  , primFactors
  , isPrime
  , divisors
  ) where

import Data.List

-- |Möbius-Funktion µ mit
--  µ(n) = (-1)^k,falls n quadratfrei, k = #Primfaktoren, 0 sonst
möbFkt :: Int -> Int
möbFkt n | facs == nub facs && even (length facs) = 1
         | facs == nub facs && odd (length facs)  = -1
         | otherwise                             = 0
  where facs = primFactors n

-- |Primfaktorzerlegung (enthält Vielfache!)
--  aus http://www.haskell.org/haskellwiki/99_questions/Solutions/35
primFactors :: Int -> [Int]
primFactors 1 = []
primFactors n = let divisors = dropWhile ((/= 0) . mod n)
                                [2 .. ceiling $ sqrt $ fromIntegral n]
           in let prime = if null divisors then n else head divisors
              in (prime :) $ primFactors $ div n prime


isPrime :: Int -> Bool
isPrime n = 1 == (length $ primFactors n)

-- |Teiler von n
divisors :: Int -> [Int]
divisors n | n == 1     = div'
           | otherwise = div' ++ [n]
  where div' = 1 : filter ((==0) . rem n) [2 .. n `div` 2]
