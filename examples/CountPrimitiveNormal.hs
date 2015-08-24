{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Main
  where
import System.CPUTime
import System.Environment
import GHC.Generics (Generic)

import Control.DeepSeq

import GalFld.GalFld
import GalFld.More.SpecialPolys

{-
 - Erzeuge die Primkörper (2,3,5,7 bereits definiert)
 -}
$(genPrimeField 11 "F11")

data T = T { ext :: Int -- Grad des Grundkörpers über dem Primkörper
           , deg :: Int -- Grad der Erweiterung
           , countP :: Int -- Anzahl primitiver Elemente
           , countN :: Int -- Anzahl normaler Elemente
           , countPN :: Int } -- Anzahl primitiv-normaler Elemente

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

genPrimNorm :: (FiniteField a, Show a, Fractional a) =>
                    Int -> Int -> a -> Int -> (T, [(Int, Polynom (FFElem a))])
genPrimNorm m n pf p = (record, fac)
  where one    = extendFFBy m pf
        cyP    = cyclotomicPoly (p^(n*m)-1) one
        piP    = piPoly $ pTupUnsave [(n,one),(0,-1)]
        ggT    = ggTP cyP piP
        fac    = factorP ggT
        record = T m n (uDegP cyP) (uDegP piP) (uDegP ggT)

main = do
  mainSub (1::F2)
  mainSub (1::F3)
  mainSub (1::F5)
  mainSub (1::F7)
  mainSub (1::F11)

mainSub pf = do
  let p = charakteristik pf
  putHeaderToFile p
  args <- getArgs
  let indxs = if' (length args == 2)
                  [(read $ head args)..(read $ head $ tail args)]
                  ( if' (length args == 1)
                        [2..(read $ head args)]
                        [2..] )
  mapM_ (\m -> (mapM_ (\n -> do
    st <- getCPUTime
    let t = fst $ genPrimNorm 1 n pf p
    putInfo t p
    {-putToFile t p-}
    putTime st) indxs)) [1..5]
      where putInfo (T m n cP cN cPN) p = do
              putStrLn $ "In F" ++ show p ++ "^" ++ show n ++ " über F" ++ show p
                ++ " gibt es:"
              putStrLn $ "\t\t" ++ show cP ++ " primitive Elemente"
              putStrLn $ "\t\t" ++ show cN ++ " normale Elemente"
              putStrLn $ "\t\t" ++ show cPN ++ " primitive und normale Elemente"
            putTime st = do
              ft <- getCPUTime
              putStrLn $ "("
                ++ show (fromIntegral (ft - st) / 1000000000000) ++ "s)\n"

            outFile p = "/tmp/CalcPrimNubers_p="++show p++".csv"
            putHeaderToFile p =
              writeFile (outFile p) "p,n,#primilive,#normal,#primitivNormal\n"
            putToFile (T m n cP cN cPN) p =
              appendFile (outFile p )$ show p ++ ","
                                     ++ show n ++ ","
                                     ++ show cP ++ ","
                                     ++ show cN ++ ","
                                     ++ show cPN ++ "\n"
