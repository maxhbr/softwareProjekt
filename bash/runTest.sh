#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
pushd $DIR
cabal configure --enable-tests \
  && cabal build -j \
  && time cabal -j test --show-details=streaming
popd
