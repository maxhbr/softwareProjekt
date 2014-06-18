#!/bin/sh
cabal update
cabal install hspec
cabal install monad-parallel
cabal intsall criterion
