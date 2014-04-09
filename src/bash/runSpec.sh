#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p $DIR/../out
ghc --make \
  -outputdir out \
  -o out/Spec \
  -threaded $DIR/../Spec.hs
$DIR/../out/Spec +RTS -N3
