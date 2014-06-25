#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
pushd $DIR
cabal configure \
  && cabal build \
  && cabal haddock
popd
