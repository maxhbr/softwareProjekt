{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main 
  where
import GalFld.GalFld
import GalFld.SpecialPolys
import System.CPUTime

$(genPrimeField 2 "PF")

pf = 1::PF
p = charakteristik pf

main = mapM_ (\n -> do
  startTime <- getCPUTime
  let cpn = countPrimNorm n
  putStr $ "Es gibt " ++ show (fst cpn)
        ++ " primitive und normale Elemente in F"
        ++show p++"^"++show n++" über F"++show p
        ++" mit Minimalpolynomen:"
  mapM_ (\(_,f) -> putStr $ "\n\t"++show f) $ snd cpn
  finishTime <- getCPUTime
  putStrLn $ " ("
    ++ show (fromIntegral (finishTime - startTime) / 1000000000000) ++ "s)") [2..]
    where countPrimNorm n = (uDegP ggT, fac)
            where cyP = cyclotomicPoly (p^n-1) pf
                  piP = piPoly $ pTupUnsave [(n,pf),(0,-1)]
                  ggT = ggTP cyP piP
                  fac = factorP ggT
