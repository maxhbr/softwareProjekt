#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
if [ ! -d "${DIR}/.cabal-sandbox/ " ]; then
  cabal sandbox init \
    && cabal install --only-dependencies -j \
    && cabal install -j \
    && cabal configure --enable-benchmarks --enable-tests
fi
