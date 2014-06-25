#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
#if [ ! -d "${DIR}/.cabal-sandbox" ]; then
if [ ! -f "${DIR}/cabal.sandbox.config" ]; then
  pushd $DIR
  cabal sandbox init \
    && cabal install --only-dependencies -j \
    && cabal configure\
    && cabal build
    #&& cabal install -j \
  popd
fi

cabal sdist
