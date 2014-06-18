--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Sandbox.SerializeSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen der serialisierung gedacht.
--
--------------------------------------------------------------------------------

module GalFld.Sandbox.SerializeSandbox
  where
import Prelude hiding (writeFile, readFile)

import Debug.Trace

import GalFld.Core
import GalFld.Algorithmen
import System.Random
import Data.List

import Data.Binary
import Data.ByteString.Lazy
--import Control.Monad
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
testPoly = testPoly1 * testPoly2 

testSerializeWrite = writeFile "/tmp/serialize" (encode testPoly)
testSerializeRead = readFile "/tmp/serialize" >>= return . decode :: IO (Polynom (FFElem F2))


main :: IO ()
{-main = print $ map fst $ sffAndBerlekamp testPoly-}
main = do
  testSerializeWrite
  r <- testSerializeRead
  print r

