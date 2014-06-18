--------------------------------------------------------------------------------
-- |
-- Module      : GalFld.Core.FiniteField
-- Note        : Finite field class
--
--
--
--------------------------------------------------------------------------------

module GalFld.Core.FiniteField
  ( FiniteField(..)
  , testAsso, testKommu, testEinh, testInv, testDist
  , testForField
  ) where
import Control.Monad

import Control.Parallel
import Control.Parallel.Strategies
-- from monad-parallel
import qualified Control.Monad.Parallel as P

import GalFld.Core.Polynomials

--------------------------------------------------------------------------------
--  Klassen Definition

class (Eq a) => FiniteField a where
  zero, one      :: a
  elems, units   :: a -> [a]
  charakteristik :: a -> Int
  elemCount      :: a -> Int
  getReprP       :: Polynom a -> a

  units x = [e | e <- elems x, e /= zero]

--------------------------------------------------------------------------------
--  Tests

testHelper f es = and $ parMap rpar f es

testAsso es = testHelper
  (\(x,y,z) -> x+(y+z)==(x+y)+z && x*(y*z)==(x*y)*z)
  [(x,y,z) | x<-es, y<-es, z<-es]

testKommu es = testHelper
  (\(x,y) -> x+y==y+x && x*y==y*x)
  [(x,y) | x<-es, y<-es]

testEinh es = testHelper
  (\x -> x+zero==x && (x*one==x || x==zero))
  es

testInv es = testHelper
  (\x -> x+(-x)==zero && (x==zero || x*recip x==one))
  es

testDist es = testHelper
  (\(x,y,z) -> x*(y+z)==(x*y)+(x*z))
  [(x,y,z) | x<-es, y<-es, z<-es]

testForField e = do
  putStrLn "teste Körper Axiome:"
  putStr "Assoziativität ist "
  putStrLn (if testAsso es then "erfüllt!" else "NICHT erfüllt!")
  putStr "Kommutativität ist "
  putStrLn (if testKommu es then "erfüllt!" else "NICHT erfüllt!")
  putStr "Einheiten sind "
  putStrLn (if testEinh es then "erfüllt!" else "NICHT erfüllt!")
  putStr "Inversen sind "
  putStrLn (if testInv es then "erfüllt!" else "NICHT erfüllt!")
  putStr "Distributivität ist "
  putStrLn (if testDist es then "erfüllt!" else "NICHT erfüllt!")
    where es = elems e
