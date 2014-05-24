{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
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

import Prelude hiding (writeFile, readFile, appendFile)
import qualified Prelude as P
import System.Environment
import Data.List

import Data.Binary
import Data.ByteString.Lazy (writeFile, readFile, appendFile)
import qualified Control.Monad.Parallel as P
import Control.Parallel
import Control.Parallel.Strategies

import Debug.Trace

import Projekt.Projekt

--------------------------------------------------------------------------------
--  Beliebiger Primkörper mit Charakteristik `c`
c = 3

data PfNumeral
instance Numeral PfNumeral where {numValue x = c}
instance Show PfNumeral where {show = show}
type PF = Mod PfNumeral

--------------------------------------------------------------------------------
--  Erweiterungen über PF

pf = 1::PF

e2fpMipo = $([|findIrred (getAllMonicPs (elems pf) [2])|])
e2pf = FFElem (P[0,pf]) e2fpMipo

e2e2pfMipo = $([|findIrred (getAllMonicPs (elems e2pf) [2])|])
e2e2pf = FFElem (P[0,e2pf]) e2e2pfMipo

e4pfMipo = $([|findIrred (getAllMonicPs (elems pf) [4])|])
e4pf = FFElem (P[0,pf]) e4pfMipo

e5e2pfMiPo = findIrred $ getAllMonicPs (elems e2pf) [5]
e5e2pf = FFElem (P[0,e2pf]) e5e2pfMiPo

e5e4pfMiPo = findIrred $ getAllMonicPs (elems e4pf) [5]
e5e4pf = FFElem (P[0,e4pf]) e5e4pfMiPo

--------------------------------------------------------------------------------
--  Problem1:
--      Finde alle irreduziblen Polynome über Endlichem Körper, welcher `e`
--      enthält, bis zu einem vorgegebenem Grad `deg`.

{-
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

  print $ ("Anzahl Irred: " ++) $ show $ length sffList

  {-
  if length bListIrred < 100
    then do print "die irreduziblen Polynome"
            mapM_ (print . snd . head) bListIrred
   -}
 -}

{-
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
 -}

{-
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
 -}

-- Gebe alle gefundenen aus
problem1d e deg = do
  print $ "Berechne monischen irred Polynome /=0 bis zu Grad "
    ++ show deg
  print $ length $ findIrreds $ getAllMonicPs (elems e) [deg]

problem1e e deg = do
  P.writeFile file ""
  print "start"
  writeFile file $ encode $ findIrreds $ getAllMonicPs (elems e) [deg]
  print "done"
    where file = "/tmp/irreds" ++ show c

problem1eMap e deg = do
  P.writeFile file ""
  print "start"
  mapM_ (appendFile file . encode) $
    findIrreds $
    getAllMonicPs (elems e) [deg]
  print "done"
    where file = "/tmp/irreds" ++ show c

problem1eRead = do
  r <- readFile file
  print (decode r:: Polynom (FFElem PF))
    where file = "/tmp/irreds" ++ show c

--------------------------------------------------------------------------------
--  Problem2:
--      Finde ein irreduziblen Polynom über Endlichem Körper, welcher `e`
--      enthält, von einem vorgegebenem Grad `deg`.

{-
problem2 e deg = do
  let es = elems e
  let list = [(toFact . aggP) f | f <- getAllMonicPs es [deg], f /= P[]]
  print $ "Anzahl aller monischen Polynome /=0 bis zu Grad "
    ++ show deg ++ ": " ++ show (length list)
  let sffList = [fs | fs <- parMap rpar appSff list, isTrivialFact fs]
  let bList = [fs | fs <- parMap rpar appBerlekamp sffList, isTrivialFact fs]

  print "Irred:"
  print $ snd $ head $ head bList
 -}

{-
problem2b e deg = do
  print "Irred:"
  print $ head irreds
    where irreds = [unFact fs | fs <- parMap rpar appBerlekamp
                     [fs | fs <- parMap rpar appSff
                           [(toFact . aggP) f | f <- getAllMonicPs (elems e) [deg]
                                              , f /= P[]]
                         , isTrivialFact fs]
                   , isTrivialFact fs]
 -}

problem2c e deg = do
  print "Irred:"
  print $ findIrred $ getAllMonicPs (elems e) deg

--------------------------------------------------------------------------------
--  Problem3:
--      Finde den Körper e5e2pf bzw. e5e4pf

problem3 = print $ length $ elems e5e2pf
problem3b = print $ length $ elems e5e4pf

--------------------------------------------------------------------------------
--  Main

dispatch :: [(String, String -> IO ())]
dispatch =  [ ("1d", \s -> problem1d e4pf (read s :: Int))
            , ("1e", \s -> problem1e e4pf (read s :: Int))
            , ("1eRead", const problem1eRead)
            , ("2c", \s -> problem2c e4pf [read s :: Int])
            , ("3", const problem3)
            , ("3b", const problem3) ]

main :: IO ()
main = do
  (grp:(arg:_)) <- getArgs
  let (Just action) = lookup grp dispatch
  action arg
