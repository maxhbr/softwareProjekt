#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
pushd $DIR
[[ -d "${DIR}/.cabal-sandbox" ]] && [[ -f "${DIR}/cabal.sandbox.config" ]] || {
  cabal sandbox init \
    && cabal install --only-dependencies -j \
    && cabal configure \
    && cabal build
}
cabal configure \
  && cabal sdist
popd
