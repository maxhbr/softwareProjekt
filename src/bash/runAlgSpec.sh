#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p $DIR/../out
ghc --make \
  -outputdir out \
  -o out/AlgSandbox \
  -threaded $DIR/../AlgSandbox.hs
$DIR/../out/AlgSandbox +RTS -N4
