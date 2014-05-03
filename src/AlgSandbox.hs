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
import Projekt.Core
import Projekt.Algorithmen
import FFSandbox (e2f2,e2e2f2,e4f2)

import SpecCommon

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

--------------------------------------------------------------------------------
testSize = 100

main :: IO ()
main = do
  list1 <- rndSelect (getAllByDegP (elems e2e2f2) 5) testSize
  list2 <- rndSelect (getAllByDegP (elems e4f2) 5) testSize
  hspec $
    describe "Projekt.Algorithmen.SFreeFactorization" $ do
      it "sff and unFact should be inverse (example f over F3)" $
        unFact (sff f) `shouldBe` f
      it "sff and unFact should be inverse (random subset of e2e2f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f) list1
      it "sff and unFact should be inverse (random subset of e4f2)" $
        pMapM_ (\f -> unFact (sff f) `shouldBe` f) list2
    --describe "Projekt.Algorithmen.Berlekamp"