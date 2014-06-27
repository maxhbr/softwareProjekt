#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
pushd $DIR
cabal configure --enable-tests \
  && cabal build \
  && time cabal test --show-details=streaming
popd
