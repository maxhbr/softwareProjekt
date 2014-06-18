#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p $DIR/../out

ghc --make \
  -outputdir out \
  -o out/HSpecTests \
  -threaded \
  -O2 \
  -fllvm \
  -rtsopts \
    $DIR/../HSpecTests.hs

if [ $? -eq 0 ]; then
  if [[ $# != 0 ]]; then
    if [[ $1 == "-N" ]]; then
      shift
      N=$1
      shift
    fi
  fi

  time $DIR/../out/HSpecTests $@ +RTS -N${N} -sstderr
fi
