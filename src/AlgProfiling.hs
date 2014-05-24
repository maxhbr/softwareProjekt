import Debug.Trace

import Projekt.Core
import Projekt.Algorithmen
import System.Random
import Data.List
import Math.Polynomial (poly, Poly, multPoly, Endianness (LE))
import Data.Matrix hiding ( (<->), (<|>))
import qualified Data.Matrix as M

{----------------------------------------------------------------------------------}
{---  Beispiele-}
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
f' = poly LE [1::F3,0,2,2,0,1,1,0,2,2,0,1] 

testPoly1 = P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                    , 1
                                    , P[1::F2,1,1]
                                    , P[0::F2,1]
                                    , P[1::F2,1,0,1] ]
testPoly2 = P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                    , 1
                                    , P[1::F2,1,0,1] ]
testPoly3 = P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                    , 1
                                    , 1
                                    , P[1::F2,1,0,1] ]
testPoly = testPoly1^2 * testPoly2 * testPoly3


multMyPoly f 1 = f
multMyPoly f n = f * multMyPoly f (n-1)

multMyPoly' f 1 = f
multMyPoly' f n = multPoly f $ multMyPoly' f (n-1)

{-m  = fromListsM [ [0::F5, 1, 0,  1, 0 ]-}
                   {-, [0, -2, 0,  0, 0 ]-}
                   {-, [0,  0, 0,  0, 0 ]-}
                   {-, [0,  0, 0, -2, 0 ]-}
                   {-, [0,  1, 0,  1, 0 ]]-}

genBigM n = replicate n $ take n $ cycle $ elems (1::F101)


m = fromListsM $ genBigM 100
m' = M.fromLists $ genBigM 100

problem1d e deg = do
  print $ "Berechne monischen irred Polynome /=0 von Grad "
    ++ show deg
  print $ length $ findIrreds $ getAllMonicPs (elems e) [deg]

prob1d e deg = map (\x -> map (\(i,f) -> berlekamp f) x) $ findTrivialsSff $ getAllMonicPs (elems e) [deg] 

fFail = fromMonomialsP [(0,1::F2),(3,1),(5,1),(7,1),(9,1)]


main :: IO ()
{-main = print $ map fst $ sffAndBerlekamp testPoly-}
{-main = print $ berlekampBasis testPoly-}
{-main = print $ multMyPoly f 400-}
{-main = print $ multMyPoly' f' 400-}
{-main = print $ multMyPoly m' 10000-}
{-main = print $ echelonM m-}
{-main = print $ luDecomp' m'-}
{-main = print $ map (\n -> (length $ prob1d (1::F2) n)) [1..10]-}
{-main = print $ prob1d (1::F2) 9-}
main = problem1d (1::F2) 9
{-main = print $ berlekamp fFail-}
