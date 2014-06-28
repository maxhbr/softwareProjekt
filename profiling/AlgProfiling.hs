{-# LANGUAGE BangPatterns #-}
module Main
  where
import Control.Arrow as A
import GalFld.GalFld
import GalFld.More.SpecialPolys
{-import System.Random-}
import Data.List
import Debug.Trace
import Data.Maybe

import GalFld.Core.Polynomials.FFTTuple
{-import GalFld.Core.Polynomials.Conway-}

{----------------------------------------------------------------------------------}
{---  Beispiele-}
e2f2Mipo = pList [1::F2,1,1] -- x²+x+1
e2f2 = FFElem (pList [0,1::F2]) e2f2Mipo


e40f2Mipo = pList [1::F2,1,0,1,0,1,0,0,1,0,0,0,1,1,0,1,1,0,1,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1]
e40f2 = FFElem (pList [0,1::F2]) e40f2Mipo

e10f3Mipo = pList [2::F3,1,0,0,2,2,2,0,0,0,1]
e10f3 = FFElem (pTupUnsave [(1,1::F3)]) e10f3Mipo


{- F16=E2(E2)
 - als Grad 2 Erweiterung von E2 durch MPol x²+x+e2f2
 - Mit einer Nullstelle: e2e2f2
 -}
e2e2f2Mipo = pList [e2f2,one,one] -- x²+x+e2f2
e2e2f2 = FFElem (pList [0,one]) e2e2f2Mipo
--e2e2f2 = FFElem (pList [0,e2f2]) e2e2f2Mipo

{- F16=E4
 - als Grad 4 Erweiterung con F2 durch MPol x⁴+x²+1
 - Mit einer Nullstelle: e4f2
 -}
e4f2Mipo = pList [1::F2,1::F2,0,0,1::F2] -- x⁴+x²+1
e4f2 = FFElem (pList [0,1::F2]) e4f2Mipo

{-
 - Beispiel in F3[x]:
 -      f = X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1
 -        = (x+1)(x²+1)³(x+2)⁴
 -}
{-f=pList [1::F3,0,2,2,0,1,1,0,2,2,0,1]-}
{-f = pTupUnsave [(11,1),(8,2::F3),(6,1),(5,1),(3,2),(2,2),(0,1)]-}
{-f' = poly LE [1::F3,0,2,2,0,1,1,0,2,2,0,1] -}
f = pTupUnsave [(11::Int,1),(8,2),(6,1),(5,1),(3,2),(2,2),(0,1)]

a = pTupUnsave [(6,3::F5),(5,2),(4,1),(3,1),(1,2),(0,3)]
b = pTupUnsave [(6,2::F5),(5,1),(4,3),(2,4)]

c = pTupUnsave [(4,1::F5)]

testPoly1 = pList $ listFFElem e40f2Mipo [ pList [0::F2,0,1,1]
                                    , 1
                                    , pList [1::F2,1,1]
                                    , pList [0::F2,1]
                                    , pList [1::F2,1,0,1] ]
testPoly2 = pList $ listFFElem e4f2Mipo [ pList [0::F2,0,1,1]
                                    , 1
                                    , pList [1::F2,1,0,1] ]
testPoly3 = pList $ listFFElem e4f2Mipo [ pList [0::F2,0,1,1]
                                    , 1
                                    , 1
                                    , pList [1::F2,1,0,1] ]
testPoly = testPoly1^2 * testPoly2 * testPoly3


testPolyF5 = pList $ listFFElem (pList [2::F5,4,1]) 
                                  [ pList [0::F5,0,1,1]
                                    , 1
                                    , pList [1::F5,1,1]
                                    , pList [0::F5,1]
                                    , pList [1::F5,1,0,1] ]

testPolyE10F3 = pList $ listFFElem e10f3Mipo [ pList [0::F3,0,1,1]
                                    , 1
                                    , pList [1::F3,1,1]
                                    , pList [0::F3,1]
                                    , pList [1::F3,1,0,1] ]

multMyPoly mulFunk f g = mulFunk f g

{-multMyPoly' f 1 = f-}
{-multMyPoly' f n = multPoly f $ multMyPoly' f (n-1)-}

{-m  = fromListsM [ [0::F5, 1, 0,  1, 0 ]-}
                   {-, [0, -2, 0,  0, 0 ]-}
                   {-, [0,  0, 0,  0, 0 ]-}
                   {-, [0,  0, 0, -2, 0 ]-}
                   {-, [0,  1, 0,  1, 0 ]]-}

genBigM n = replicate n $ take n $ cycle $ elems (1::F101)


m = fromListsM $ genBigM 100
{-m' = M.fromLists $ genBigM 100-}

problem1d e deg = do
  print $ "Berechne monischen irred Polynome /=0 von Grad "
    ++ show deg
  print $ length $ findIrreds $ getAllMonicPs (elems e) [deg]

prob1d e deg = map (\x -> map (\(i,f) -> berlekamp f) x) $ findTrivialsSff $ getAllMonicPs (elems e) [deg] 


l = take 100 $ getAllMonicPs (elems (1::F3)) [100]


heavyBench mul f 0 = f
heavyBench mul f n = mul f g
  where g = heavyBench mul f (n-1) 

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
{-main = problem1d (1::F2) 13-}
{-main = print $ berlekamp fFail-}
{-main = print $ length $ filter (\x -> x) $ map (\f -> rabin f) $ getAllMonicPs (elems (1::F3)) [8]-}
{-main = print $ map (\f -> hasNs f (elems (1::F3))) $ getAllMonicPs (elems (1::F3)) [2]-}
{-main = mapM_ print $ map appBerlekamp $ map appSff $ findTrivialsNs $ getAllMonicPs (elems (1::F3)) [2]-}
{-main = print $ length $ filter (\x -> x) $ map (rabin . toPMS) $ getAllMonicPs (elems (1::F3)) [8]-}
{-main = print $ length $ findIrreds $ getAllMonicPs (elems (1::F3)) [9]-}
{-main = mapM_ print $ map sffAndBerlekamp $ getAllMonicPs (elems (1::F3)) [3]-}
{-main = print $ length $ findIrredsRabin $ getAllMonicPs (elems (1::F3)) [9]-}
{-main = print $ snd $ (divPInv (pTupUnsave [(3^11,1),(1,-1)]) f)-}
{-main = print $ foldr1 (+) $ map (snd) $ p2Tup $ heavyBench testPoly1 200-}
{-main = print $ foldr1 (+) $ map (snd) $ heavyBench (p2Tup testPoly1) 200-}
{-main = do-}
  {-print $ divP (pTup [(3^20,1::F3), (0,-1)]) (pTup [(40,1::F3), (1,1),(0,2)])-}
{-main = print $ multPMKaratsuba (p2Tup (testPolyF5^1000)) (p2Tup (testPolyF5^1000))-}
{-main = print $ foldr1 (+) $ map snd $ p2Tup $ heavyBench (multPK) testPolyF5 300-}
{-main = print $ modMonom (5^21) a-}
{-main = print $ m-}
  {-where m = factorP $ ggTP (piPoly $ pTupUnsave [(5,1::F3),(0,-1)]) (cyclotomicPoly (3^5-1) (1::F3))-}
main = do
  let list = getAllP (elems (1::F5)) 8
  putStrLn $ (\(f,g,a,b) -> "f="++show f++"\ng="++show g++"\ndivP f g = "++show a++"\ndivPInv="++show b) $ 
               head $ filter (\(_,_,a,b) -> a /= b) $ 
               map (\(x,y) -> (x,y,divP x y, divPInv x y)) $ 
               zip list $ reverse list
