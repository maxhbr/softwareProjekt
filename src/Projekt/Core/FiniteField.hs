--------------------------------------------------------------------------------
-- |
-- Module      : Project.Core.FiniteField
-- Note        : Finite field class
--
--
--
--------------------------------------------------------------------------------

module Projekt.Core.FiniteField
  ( FiniteField(..)
  , testAsso, testKommu, testEinh, testInv, testDist
  , testForField
  ) where
-- from monad-parallel
import qualified Control.Monad.Parallel as P

pMapM_  f = P.sequence_ . map f

class (Eq a) => FiniteField a where
  zero, one    :: a
  elems, units :: a -> [a]

  units x = [e | e <- elems x, e /= zero]

testHelper f s = do 
  bs <- P.mapM f s
  return (and bs)

testAsso es = testHelper
  (\(x,y,z) -> return $ x+(y+z)==(x+y)+z && x*(y*z)==(x*y)*z)
  [(x,y,z) | x<-es, y<-es, z<-es]
testKommu es = testHelper
  (\(x,y) -> return $ x+y==y+x && x*y==y*x)
  [(x,y) | x<-es, y<-es]
testEinh es = testHelper
  (\x -> return $ x+zero==x && (x*one==x || x==zero))
  es
testInv es = testHelper
  (\x -> return $ x+(-x)==zero && (x==zero || x*recip x==one))
  es
testDist es = testHelper
  (\(x,y,z) -> return $ x*(y+z)==(x*y)+(x*z))
  [(x,y,z) | x<-es, y<-es, z<-es]

testForField e = do
  putStrLn "Teste Körper Axiome:"
  putStr "Assoziativität ist "
  t <- testAsso es
  let r = t
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Kommutativität ist "
  t <- testKommu es
  let r = r && t
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Einheiten sind "
  t <- testEinh es
  let r = r && t
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Inversen sind "
  t <- testInv es
  let r = r && t
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Distributivität ist "
  t <- testDist es
  let r = r && t
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  return r
    where es = elems e

