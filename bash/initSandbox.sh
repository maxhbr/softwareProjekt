#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
pushd $DIR
#if [ ! -d "${DIR}/.cabal-sandbox" ]; then
if [ ! -f "${DIR}/cabal.sandbox.config" ]; then
  cabal sandbox init \
    && cabal install --only-dependencies -j \
    && cabal configure \
    && cabal build
    #&& cabal install -j \
fi

cabal configure \
  && cabal sdist
popd
