import GalFld.Core
import GalFld.SpecialPolys

pf = 1::F2
p = 2

main = do
    mapM_ (\n -> putStrLn $ "Es gibt " ++ show (countPrimNorm n) 
            ++ " primitive und normale Elemente in F"++show n++" über F"++show p) 
          [2..]
      where countPrimNorm n = (length fac) * deg
            cyP = cyclotomicPoly (p^n-1) pf
            piP = piPoly $ pTupUnsave [(n,pf),(0,-1)]
            fac = factorP $ ggTP cyP piP
            deg = uDegP $ snd $ head fac
