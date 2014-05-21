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
import qualified Control.Monad.Parallel as P

--------------------------------------------------------------------------------
--  Beliebiger Primkörper

data PfNumeral
instance Numeral PfNumeral where
  numValue x = 2 -- setze die Charakteristik
instance Show PfNumeral where
  show = show
type PF = Mod PfNumeral

--------------------------------------------------------------------------------
--  Erweiterungen über F2

f2 = 1::F2

e2f2Mipo = P[1::F2,1,1] -- x²+x+1
e2f2 = FFElem (P[0,1::F2]) e2f2Mipo

e2e2f2Mipo = P[e2f2,one,one] -- x²+x+e2f2
e2e2f2 = FFElem (P[0,one]) e2e2f2Mipo

--------------------------------------------------------------------------------
--  Main

e = e2f2
deg = 4

main :: IO ()
main = do
  print "Anzahl aller Elemente im Galoiskörper:"
  let es = elems e
  print $ length es

  print $ "Anzahl aller Polynomeb /=0 bis zu Grad " ++ show deg ++ ":"
  let list = [(toFact . aggP) f | f <- getAllP es deg, f /= P[]]
  print $ length list

  print "wende SFF an:"
  sffList <- P.mapM (return . appSff) list
  let sffListIrred = [fs | fs <- sffList , isTrivialFact fs]
  print $ length sffListIrred

  print "wende Berlekamp an:"
  bList <- P.mapM (return . appBerlekamp) list
  let bListIrred = [fs | fs <- bList , isTrivialFact fs]
  print $ length bListIrred

  {-
  if length bListIrred < 100
    then do print "die irreduziblen Polynome"
            mapM_ (print . snd . head) bListIrred
   -}
