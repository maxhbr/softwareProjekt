import GalFld.GalFld
import GalFld.SpecialPolys

pf = 1::F2
p = charakteristik pf

main = do
    mapM_ (\n -> putStrLn $ "Es gibt " ++ show (countPrimNorm n) 
            ++ " primitive und normale Elemente in F"
            ++show p++"^"++show n++" über F"++show p) 
          [2..]
      where countPrimNorm n = uDegP ggT
              where cyP = cyclotomicPoly (p^n-1) pf
                    piP = piPoly $ pTupUnsave [(n,pf),(0,-1)]
                    ggT = ggTP cyP piP 
                    fac = factorP $ ggT
                    deg = uDegP $ snd $ head fac
