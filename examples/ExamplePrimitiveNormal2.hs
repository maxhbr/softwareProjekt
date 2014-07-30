{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  where

import GalFld.GalFld
import GalFld.More.SpecialPolys


data T = T { deg :: Int -- Grad der Erweiterung
           , countP :: Int -- Anzahl primitiver Elemente
           , countN :: Int -- Anzahl normaler Elemente
           , countPN :: Int } -- Anzahl primitiv-normaler Elemente



genPrimNorm :: (FiniteField a) => a -> Int -> T
genPrimNorm pf n = (record, fac)
  where cyP    = cyclotomicPoly (p^n-1) pf
        piP    = piPoly $ pTupUnsave [(n,pf),(0,-1)]
        ggT    = ggTP cyP piP
        fac    = factorP ggT
        record = T n (uDegP cyP) (uDegP piP) (uDegP ggT)
        p      = charakteristik pf


main = do
  mapM_ (\p -> do
    $(genPrimeField p "PF")
    let pf = 1::PF
    putStrLn $ "p = " ++ show p
    mapM_ (\n -> do
      let gpn = genPrimNorm pf n
      putInfo gpn) [1..10]
    ) [2,3,5,7,11,13]
      where putInfo (T n cP cN cPN) = do
              putStrLn $ show n ++ "\t" ++ show cP ++ "\t" 
                          ++ show cN ++ "\t" ++ show cPN

    

