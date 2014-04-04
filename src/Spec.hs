import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Projekt.Core

--------------------------------------------------------------------------------
--  Globale Charakteristik
charakteristik :: Integer
charakteristik = 2

--------------------------------------------------------------------------------
--  Definiere Endlichen Körper
data PeanoNumber
instance Numeral PeanoNumber where numValue x = charakteristik
instance Show PeanoNumber    where show       = show
type PF = Mod PeanoNumber

--------------------------------------------------------------------------------
ffVMipo = P [(2,1::PF),(1,1::PF),(0,1::PF)]

ff1 = FFKonst (1::PF)
ffV = FFElem (P[(1,1::PF)]) ffVMipo

ffElems = elems ffV

--------------------------------------------------------------------------------
fffVMipo = P [(2,ff1),(1,ff1),(0,ffV)]

fff1 = FFKonst $ FFKonst (1::PF)
fffV = FFElem (P [(1,ff1)]) fffVMipo

fffElems = elems fffV

--------------------------------------------------------------------------------
-- TODO QuickCheck
{-
instance (FFElem a) => Arbitrary (FFElem a) where
  arbitrary     = choose (fff1,fffV)
 -}

unEekP (d,s,t) a b = d == s*a + t*b

--------------------------------------------------------------------------------
main :: IO ()
main = hspec $ do
  describe "Projekt.Core.PrimeFields" $ do
    it "modulus should give the modulus" $ do
      modulus (2 ::Z2) `shouldBe` 2
      modulus (2 ::Z3) `shouldBe` 3
      modulus (2 ::Z5) `shouldBe` 5
      modulus (2 ::Z101) `shouldBe` 101
    it "test Eq" $ do
      (2 ::Z2) `shouldBe` (0 ::Z2)
      (2 ::Z2) `shouldBe` (4 ::Z2)
    it "test for neutral element in Z101" $ mapM_
      (\ x -> x * (1::Z101) `shouldBe` x)
      (units undefined ::[Z101])
    it "x/0 throws exception" $
      evaluate ((3::Z101) / (0::Z101)) `shouldThrow` anyException
    it "0/0 throws exception" $
      evaluate ((0::Z101) / (0::Z101)) `shouldThrow` anyException
    it "test invMod (x/x=1) in Z101" $ mapM_
      (\ x -> x / x `shouldBe` (1 ::Z101))
      (units undefined ::[Z101])
    it "test invMod (x/x*x=x) in Z101" $ mapM_
      (\ x -> x / x * x `shouldBe` x)
      (units undefined ::[Z101])

  describe "Projekt.Core.Polynomials" $
    it "test eekP over Z101" $ mapM_
      (\ (x, y) -> unEekP (eekP x y) x y `shouldBe` True)
      (zip (reverse $ getAllP (elems undefined ::[Z5]) 5) $
           getAllP (elems undefined ::[Z5]) 3)

  describe "Projekt.Core.FiniteFields @ffV" $ do
    it "test for neutral element" $ mapM_
      (\ x -> x * ff1 `shouldBe` x)
      ffElems
    it "x/0 throws exception" $ do
      evaluate (ff1 / FFElem (P[]) ffVMipo) `shouldThrow` anyException
      evaluate (ffV / FFElem (P[]) ffVMipo) `shouldThrow` anyException
    it "0/0 throws exception" $
      evaluate (FFElem (P[]) ffVMipo / FFElem (P[]) ffVMipo) `shouldThrow` anyException
    it "test invMod (x/x=1)" $ mapM_
      (\ x -> x / x `shouldBe` ff1)
      (units ffV)
    it "test invMod (x/x*x=x)" $ mapM_
      (\ x -> x / x * x `shouldBe` x)
      (units ffV)

  describe "Projekt.Core.FiniteFields @fffV" $ do
    it "test for neutral element" $ mapM_
      (\ x -> x * fff1 `shouldBe` x)
      fffElems
    it "x/0 throws exception" $ do
      evaluate (fff1 / FFElem (P[]) fffVMipo) `shouldThrow` anyException
      evaluate (fffV / FFElem (P[]) fffVMipo) `shouldThrow` anyException
    it "0/0 throws exception" $
      evaluate (FFElem (P[]) fffVMipo / FFElem (P[]) fffVMipo) `shouldThrow` anyException
    it "test invMod (x/x=1)" $ mapM_
      (\ x -> x / x `shouldBe` fff1)
      (units fffV)
    it "test invMod (x/x*x=x)" $ mapM_
      (\ x -> x / x * x `shouldBe` x)
      (units fffV)

    {-
    it "general inversion" $
      property $ \x -> x / x * x == (x::FFElem a)
     -}
