import Debug.Trace

import Projekt.Core
import Projekt.Algorithmen
import System.Random
--------------------------------------------------------------------------------
--  Beispiele
e2f2Mipo = P[1::F2,1,1] -- x²+x+1
e2f2 = FFElem (P[0,1::F2]) e2f2Mipo

{- F16=E2(E2)
 - als Grad 2 Erweiterung von E2 durch MPol x²+x+e2f2
 - Mit einer Nullstelle: e2e2f2
 -}
e2e2f2Mipo = P[e2f2,one,one] -- x²+x+e2f2
e2e2f2 = FFElem (P[0,one]) e2e2f2Mipo
--e2e2f2 = FFElem (P[0,e2f2]) e2e2f2Mipo

{- F16=E4
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle: e4f2
 -}
e4f2Mipo = P[1::F2,1::F2,0,0,1::F2] -- x⁴+x²+1
e4f2 = FFElem (P[0,1::F2]) e4f2Mipo

{-
 - Beispiel in F3[x]:
 -      f = X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1
 -        = (x+1)(x²+1)³(x+2)⁴
 -}
f=P[1::F3,0,2,2,0,1,1,0,2,2,0,1]

testPoly = (P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                    , 1
                                    , P[1::F2,1,1]
                                    , P[0::F2,1]
                                    , P[1::F2,1,0,1] ])^2
         * (P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                    , 1
                                    , P[1::F2,1,0,1] ])
         * (P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                    , 1
                                    , 1
                                    , P[1::F2,1,0,1] ])

main :: IO ()
main = do
  {-gen <- getStdGen-}
  {-let xs = getAllByDegP (elems e2e2f2) 4-}
  {-print $ length xs -- 61440-}
  -- print $ sffAndBerlekamp $ xs!!(head $ randomRs (0, length xs - 1) gen)
  {-print $ map fst $ sffAndBerlekamp $ xs!!(quot 61440 2 + i)-}
  print $ map fst $ sffAndBerlekamp testPoly

