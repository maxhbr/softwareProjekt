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

import Prelude hiding (writeFile)

import Projekt.Core
import Projekt.Algorithmen
import Debug.Trace
import qualified Control.Monad.Parallel as P
{-import Control.Concurrent.Async (mapConcurrently)-}
import Control.Parallel
import Control.Parallel.Strategies

import Data.Binary
import Data.ByteString.Lazy (writeFile)

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
e2e2f2 = FFElem (P[0,e2f2]) e2e2f2Mipo

e4f2Mipo = P[1::F2,1::F2,0,0,1::F2] -- x⁴+x²+1
e4f2 = FFElem (P[0,1::F2]) e4f2Mipo

e5e2f2MiPo = findIrred $ getAllMonicPs (elems e2f2) [5]
e5e2f2 = FFElem (P[0,e2f2]) e5e2f2MiPo

e5e4f2MiPo = findIrred $ getAllMonicPs (elems e4f2) [5]
e5e4f2 = FFElem (P[0,e4f2]) e5e4f2MiPo

--------------------------------------------------------------------------------
--  Problem1:
--      Finde alle irreduziblen Polynome über Endlichem Körper, welcher `e`
--      enthält, bis zu einem vorgegebenem Grad `deg`.

problem1 e deg = do
  let es = elems e
  print $ ("Anzahl aller Elemente im Galoiskörper: " ++) $ show $ length es

  let list = [(toFact . aggP) f | f <- getAllMonicP es deg, f /= P[]]
  print $ "Anzahl aller monischen Polynome /=0 bis zu Grad "
    ++ show deg ++ ": " ++ show (length list)

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

-- Speicher gefundene als Liste in eine Datei
problem1b e deg = do 
  print $ "Berechne monischen irred Polynome /=0 bis zu Grad "
    ++ show deg
  writeFile "/tmp/irreds" (encode irreds)
  print $ ("Anzahl Irred: " ++) $ show $ length irreds
    where irreds = [unFact fs | fs <- parMap rpar appBerlekamp
                     [fs | fs <- parMap rpar appSff
                           [(toFact . aggP) f | f <- getAllMonicPs (elems e) [deg]
                                              , f /= P[]]
                         , isTrivialFact fs]
                   , isTrivialFact fs]

-- Gebe alle gefundenen aus
problem1c e deg = do
  print $ "Berechne monischen irred Polynome /=0 bis zu Grad "
    ++ show deg
  mapM_ print irreds
  print $ ("Anzahl Irred: " ++) $ show $ length irreds
    where irreds = [unFact fs | fs <- parMap rpar appBerlekamp
                     [fs | fs <- parMap rpar appSff
                           [(toFact . aggP) f | f <- getAllMonicPs (elems e) [deg]
                                              , f /= P[]]
                         , isTrivialFact fs]
                   , isTrivialFact fs]

--------------------------------------------------------------------------------
--  Problem2:
--      Finde ein irreduziblen Polynom über Endlichem Körper, welcher `e`
--      enthält, von einem vorgegebenem Grad `deg`.

problem2 e deg = do
  let es = elems e
  let list = [(toFact . aggP) f | f <- getAllMonicPs es [deg], f /= P[]]
  print $ "Anzahl aller monischen Polynome /=0 bis zu Grad "
    ++ show deg ++ ": " ++ show (length list)
  let sffList = [fs | fs <- parMap rpar appSff list, isTrivialFact fs]
  let bList = [fs | fs <- parMap rpar appBerlekamp sffList, isTrivialFact fs]

  print "Irred:"
  print $ snd $ head $ head bList

problem2b e deg = do
  print "Irred:"
  print $ head irreds
    where irreds = [unFact fs | fs <- parMap rpar appBerlekamp
                     [fs | fs <- parMap rpar appSff
                           [(toFact . aggP) f | f <- getAllMonicPs (elems e) [deg]
                                              , f /= P[]]
                         , isTrivialFact fs]
                   , isTrivialFact fs]

problem2c e deg = do
  print "Irred:"
  print $ findIrred $ getAllMonicPs (elems e) [deg]

--------------------------------------------------------------------------------
--  Problem3:
--      Finde den Körper e5e2f2 bzw. e5e4f2

problem3 = print $ length $ elems e5e2f2
problem3b = print $ length $ elems e5e4f2

--------------------------------------------------------------------------------
--  Main

main :: IO ()
main = problem1b e2f2 5
{-main = problem2c e2f2 5-}
{-main = problem3-}
