#!/bin/bash
################################################################################
# usage:
#    cPN.sh
#    cPN.sh TOP
#    cPN.sh TOP "PRIME1 PRIME2 ..."
#
# Last modified: Sat Aug 16, 2014  06:44

###############################################################################
# Config
PATHBASE="/tmp/cPN/"
if [ $# -eq 2 ]; then
  LIMIT=$1
  LIST=$2
elif [ $# -eq 2 ]; then
  LIST="2 3 5 7 11 13 17"
  LIMIT=$1
else
  LIST="2 3 5 7 11 13 17"
  LIMIT=8
fi

###############################################################################
FAIL=0
mkdir -p $PATHBASE
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p "${DIR}/cPNout/"
SRC="${DIR}/../src/"

genPathHS(){
  echo "${SRC}cPN${1}.hs"
}
genPathEx(){
  echo "${PATHBASE}cPN${1}"
}
genPathCSV(){
  echo "${DIR}/cPNout/CalcPrimNubers_p=${1}.csv"
}

genHS(){
  # genHS CHARAKTERISTIK BEGINN END
cat <<HS
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Main
  where
import System.Environment
import System.Directory
import Control.Monad

import GalFld.GalFld
import GalFld.More.SpecialPolys

p = ${1}
\$(genPrimeField ${1} "PF")
outFile = "$(genPathCSV $1)"

pf = 1::PF

data T = T { deg :: Int -- Grad der Erweiterung
           , countP :: Int -- Anzahl primitiver Elemente
           , countN :: Int -- Anzahl normaler Elemente
           , countPN :: Int } -- Anzahl primitiv-normaler Elemente

genPrimNorm :: Int -> (T, [(Int, Polynom PF)])
genPrimNorm n = (record, fac)
  where cyP    = cyclotomicPoly (p^n-1) pf
        piP    = piPoly $ pTupUnsave [(n,pf),(0,-1)]
        ggT    = ggTP cyP piP
        fac    = factorP ggT
        record = T n (uDegP cyP) (uDegP piP) (uDegP ggT)

main = do
  args <- getArgs
  let indxs = [(read $ head args)..(read $ head $ tail args)]
  bool <- doesFileExist outFile
  unless bool $ putHeaderToFile p
  mapM_ (\n -> do
    let t = fst $ genPrimNorm n
    putInfo t p
    putToFile t p) indxs
      where putInfo !(T n cP cN cPN) p = do
              putStrLn $ "In F" ++ show p ++ "^" ++ show n ++ " über F" ++ show p
                ++ " gibt es:"
                ++ "\t" ++ show cP
                ++ "\t" ++ show cN
                ++ "\t" ++ show cPN
            putHeaderToFile p =
              writeFile outFile "p,n,#primilive,#normal,#primitivNormal\n"
            putToFile (T n cP cN cPN) p =
              appendFile outFile $ show p ++ ","
                                     ++ show n ++ ","
                                     ++ show cP ++ ","
                                     ++ show cN ++ ","
                                     ++ show cPN ++ "\n"

HS
}

getDoneNumber(){
  csv=$(genPathCSV $1)
  if [ -f $csv ]; then
    echo `expr $(tail -n1 $csv | cut -d',' -f2) + 1`
  else
    echo "1"
  fi
}

###############################################################################
# gen and compile HS
pushd $SRC
for p in $LIST; do
  if [ ! -f $(genPathEx $p) ]; then
    genHS $p >$(genPathHS $p)
    mkdir -p "${PATHBASE}compileOut/"
    ghc \
      -outputdir "${PATHBASE}compileOut/" \
      -o "$(genPathEx $p)" \
      -O2 -isrc -threaded \
      "$(genPathHS $p)"
    rm $(genPathHS $p)
  fi
done
popd

###############################################################################
for p in $LIST; do
  if [ -f $(genPathEx $p) ]; then
    #echo $(getDoneNumber $p)
    $(genPathEx $p) $(getDoneNumber $p) $LIMIT &
  fi

  # limit subprocesses to 4
  while [ $(jobs -p | wc -l) -gt 3 ] ; do
    sleep 1
  done
done

###############################################################################
for job in `jobs -p` ; do
  wait $job || let "FAIL+=1"
done
if [ $FAIL -gt 0 ] ; then
  echo "fails: $FAIL"
fi
