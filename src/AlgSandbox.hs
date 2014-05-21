--------------------------------------------------------------------------------
-- |
-- Module      : AlgSandbox
-- Note        : Beispiele und Platz zum Spielen und Probieren
--
--  Diese Sandbox ist zum testen von FiniteFields gedacht.
--
--------------------------------------------------------------------------------

module AlgSandbox
  where

import Debug.Trace

import Projekt.Core
import Projekt.Algorithmen
import FFSandbox (e2f2,e2e2f2,e4f2,e4f2Mipo)

import SpecCommon
import PolySandbox hiding (testSize, main)

--------------------------------------------------------------------------------
--  Beispiele

{-
 - Beispiel in F3[x]:
 -      f = X¹¹+2x⁹+2x⁸+x⁶+x⁵+2x³+2x²+1
 -        = (x+1)(x²+1)³(x+2)⁴
 -}
f=P[1::F3,0,2,2,0,1,1,0,2,2,0,1]
sqf=[(1,P[1::F3,1])
    ,(3,P[1::F3,0,1])
    ,(4,P[2::F3,1])]


pp :: (Show a) => [a] -> IO()
pp =  mapM_ print

f' = P[4::F5, 1, 0, 0, 4, 1]

f'' = P[1::F2, 1, 1, 1]

f''' = P[1::F2,0,0,0,1,1]

--------------------------------------------------------------------------------
-- | failing f:
--  (1₂·X³+1₂·X+1₂ mod 1₂·X⁴+1₂·X+1₂)·X⁴
-- +(1₂·X          mod 1₂·X⁴+1₂·X+1₂)·X³
-- +(1₂·X²+1₂·X+1₂ mod 1₂·X⁴+1₂·X+1₂)·X²
-- +(1₂            mod 1₂·X⁴+1₂·X+1₂)·X
-- +(1₂·X³+1₂·X²   mod 1₂·X⁴+1₂·X+1₂)
sffFailF = P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
                                , 1
                                , P[1::F2,1,1]
                                , P[0::F2,1]
                                , P[1::F2,1,0,1] ]

--------------------------------------------------------------------------------
-- |Böses Polynom für Berlekamp (f geht auch, da bFailF ein Teiler von f ist)
bFailF = P[1::F3,0,1]

bFailF2 = (P $ listFFElem e4f2Mipo [ P[0::F2,0,1,1]
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

--------------------------------------------------------------------------------
testSize = 100

main :: IO ()
main = do
  list1 <- rndSelect (getAllPs (elems e2e2f2) [5,4]) testSize
  list2 <- rndSelect (getAllPs (elems e4f2) [5,4]) testSize
  hspec $ do
    describe "Projekt.Algorithmen.SFreeFactorization" $ do
      it "sff and unFact should be inverse (example f over F3)" $
        unFact (sff f) `shouldBe` f
      it "sff and unFact should be inverse (up to deg 10 of f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f) 
        (getAllP (elems (0::F2)) 10)
      it "sff and unFact should be inverse (up to deg 7 of e2f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f) 
        (getAllP (elems e2f2) 7)
      it "sff and unFact should be inverse (random subset of e2e2f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f) list1
      it "sff and unFact should be inverse (random subset of e4f2)" $
        mapM_ (\f -> unFact (sff f) `shouldBe` f) list2
    describe "Projekt.Algorithmen.Berlekamp" $ do
      it "sffAndBerlekamp and unFact should be inverse (example f over F3)" $
        unFact (sffAndBerlekamp f) `shouldBe` f
      it "sffAndBerlekamp and unFact should be inverse (example bFailF)" $
        unFact (sffAndBerlekamp bFailF) `shouldBe` bFailF
      it "sffAndBerlekamp and unFact should be inverse (example sffFailF)" $
        unFact (sffAndBerlekamp sffFailF) `shouldBe` sffFailF
      it "sffAndBerlekamp and unFact should be inverse (random subset of e2e2f2)" $
        pMapM_ (\f -> unFact (sffAndBerlekamp f) `shouldBe` f) $
        take (quot testSize 50 + 1) list1
      it "sffAndBerlekamp and unFact should be inverse (random subset of e4f2)" $
        mapM_ (\f -> unFact (sffAndBerlekamp f) `shouldBe` f) $
        take (quot testSize 50 + 1) list2
