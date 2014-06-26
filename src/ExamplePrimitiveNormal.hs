{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Mail where
import GalFld.GalFld
import GalFld.SpecialPolys
import System.CPUTime

$(genPrimeField 2 "PF")

pf = 1::PF
p = charakteristik pf

main = mapM_ (\n -> do
  startTime <- getCPUTime
  putStr $ "Es gibt " ++ show (countPrimNorm n)
        ++ " primitive und normale Elemente in F"
        ++show p++"^"++show n++" über F"++show p
  finishTime <- getCPUTime
  putStrLn $ " ("
    ++ show (fromIntegral (finishTime - startTime) / 1000000000000) ++ "s)") [2..]
    where countPrimNorm n = uDegP $ ggTP cyP piP
            where cyP = cyclotomicPoly (p^n-1) pf
                  piP = piPoly $ pTupUnsave [(n,pf),(0,-1)]
                  --fac = factorP ggT
                  --deg = uDegP $ snd $ head fac
