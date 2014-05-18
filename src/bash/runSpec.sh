#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
mkdir -p $DIR/../out

ghc --make \
  -outputdir out \
  -o out/Spec \
  -threaded \
  -fforce-recomp \
  -O2 \
  -fllvm \
    $DIR/../Spec.hs

if [ $? -eq 0 ]; then
  $DIR/../out/Spec +RTS -N4
fi

# -fforce-recomp, -O2 and -fllvm are from:
# stackoverflow.com/questions/7017116/haskell-simple-way-to-cache-a-function-call
