--------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Note        :
--
--
--
--------------------------------------------------------------------------------
module Main
  where

import Projekt.Core
import Projekt.Algorithmen
import Debug.Trace

data PfNumeral
instance Numeral PfNumeral where
  numValue x = 2 -- setze die Charakteristik
instance Show PfNumeral where
  show = show
type PF = Mod PfNumeral

deg = 3
main :: IO ()
main = do
  print "Anzahl aller Elemente im Galoiskörper:"
  let es = elems undefined ::[PF]
  print $ length es

  print $ "Anzahl aller Polynomeb /=0 bis zu Grad " ++ show deg ++ ":"
  let list = [(toFact . aggP) f | f <- getAllP es deg, f /= P[]]
  print $ length list

  print "wende SFF an:"
  let sffList = map appSff list
  print $ length [fs | fs <- sffList , isTrivialFact fs]

  print "wende Berlekamp an:"
  let bList = map appBerlekamp sffList
  print $ length [fs | fs <- bList , isTrivialFact fs]

  print "die irreduziblen Polynome"
  mapM_ (print . snd . head) [fs | fs <- bList , isTrivialFact fs]
