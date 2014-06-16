#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p $DIR/../out

ghc --make \
  -outputdir out \
  -o out/Bench \
  -threaded \
  -O2 \
  -fllvm \
    $DIR/../Projekt/Tests/Bench.hs
  #-fforce-recomp \

if [ $? -eq 0 ]; then
  if [[ $# != 0 ]]; then
    if [[ $1 == "-N" ]]; then
      shift
      N=$1
      shift
    fi
  fi

  time $DIR/../out/Bench $@ +RTS -N${N}
fi

# -fforce-recomp, -O2 and -fllvm are from:
# stackoverflow.com/questions/7017116/haskell-simple-way-to-cache-a-function-call
