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
import Control.Monad

-- from monad-parallel
-- TODO: remove?
import qualified Control.Monad.Parallel as P

import Projekt.Core.Polynomials

--------------------------------------------------------------------------------
--  Klassen Definition

class (Eq a) => FiniteField a where
  zero, one      :: a
  elems, units   :: a -> [a]
  charakteristik :: a -> Int
  elemCount      :: a -> Int
  getReprP       :: Polynom a -> a

  units x = [e | e <- elems x, e /= zero]
  getReprP (P []) = error "Insufficient information"

--------------------------------------------------------------------------------
--  Tests

pMapM_  f = P.sequence_ . map f

testHelper f s = liftM and (P.mapM f s)

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
  putStrLn "teste Körper Axiome:"
  putStr "Assoziativität ist "
  t <- testAsso es
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Kommutativität ist "
  t <- testKommu es
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Einheiten sind "
  t <- testEinh es
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Inversen sind "
  t <- testInv es
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
  putStr "Distributivität ist "
  t <- testDist es
  putStrLn (if t then "erfüllt!" else "NICHT erfüllt!")
    where es = elems e

