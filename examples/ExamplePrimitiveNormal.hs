{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main
  where
import GalFld.GalFld
import GalFld.More.SpecialPolys
import System.CPUTime

$(genPrimeField 2 "PF")

pf = 1::PF
p = charakteristik pf

data T = T { deg :: Int -- Grad der Erweiterung
           , countP :: Int -- Anzahl primitiver Elemente
           , countN :: Int -- Anzahl normaler Elemente
           , countPN :: Int } -- Anzahl primitivNormaler Elemente

genPrimNorm n = (record, fac)
  where cyP    = cyclotomicPoly (p^n-1) pf
        piP    = piPoly $ pTupUnsave [(n,pf),(0,-1)]
        ggT    = ggTP cyP piP
        fac    = factorP ggT
        record = T n (uDegP cyP) (uDegP piP) (uDegP ggT)

main = mapM_ (\n -> do
  st <- getCPUTime
  let gpn = genPrimNorm n
  putInfo $ fst gpn
  putPolys $ snd gpn
  putTime st ) [2..9]
    where putInfo (T n cP cN cPN) = do
            putStrLn $ "In F" ++ show p ++ "^" ++ show n ++ " über F" ++ show p
              ++ " gibt es:"
            putStrLn $ "\t\t" ++ show cP ++ " primitive Elemente"
            putStrLn $ "\t\t" ++ show cN ++ " normale Elemente"
            putStrLn $ "\t\t" ++ show cPN ++ " primitive und normale Elemente"
          putPolys fs = do
            putStrLn "Mit Minimalpolynomen:"
            mapM_ (\(_,f) -> putStrLn $ "\t" ++ show f) fs
          putTime st = do
            ft <- getCPUTime
            putStrLn $ "("
              ++ show (fromIntegral (ft - st) / 1000000000000) ++ "s)\n"
