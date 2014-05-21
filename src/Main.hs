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
{-import qualified Control.Monad.Parallel as P-}
{-import Control.Concurrent.Async (mapConcurrently)-}
import Control.Parallel
import Control.Parallel.Strategies

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

e4f2Mipo = P[1::F2,1::F2,0,0,1::F2] -- x⁴+x²+1
e4f2 = FFElem (P[0,1::F2]) e4f2Mipo

--------------------------------------------------------------------------------
--  Problem1:
--      Finde alle irreduziblen Polynome über Endlichem Körper, welcher `e`
--      enthält, bis zu einem vorgegebenem Grad `deg`.

problem1 e deg = do
  let es = elems e
  print $ ("Anzahl aller Elemente im Galoiskörper: " ++) $ show $ length es

  let list = [(toFact . aggP) f | f <- getAllMonicP es deg, f /= P[]]
  print $ "Anzahl aller monischen Polynome /=0 bis zu Grad " 
    ++ (show deg) ++ ": " ++ (show $ length list)

  print "Suche Irred!"

  -- print "wende SFF an:"
  {-let sffList = parMap rpar (\(f,i) -> trace ("sff " ++ show i) (appSff f)) (zip list [1..])-}
  let sffList = [fs | fs <- parMap rpar appSff list, isTrivialFact fs]

  -- print "wende Berlekamp an:"
  {-let bList = parMap rpar (\(f,i) -> trace ("b " ++ show i) (appBerlekamp f)) (zip sffList [1..])-}
  let bList = [fs | fs <- parMap rpar appBerlekamp sffList, isTrivialFact fs]

  print $ ("Anzahl Irred: " ++) $ show $ length bList

  {-
  if length bListIrred < 100
    then do print "die irreduziblen Polynome"
            mapM_ (print . snd . head) bListIrred
   -}

--------------------------------------------------------------------------------
--  Main

e = e4f2
deg = 4

main :: IO ()
main = problem1 e deg
