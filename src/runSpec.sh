#!/bin/sh

mkdir -p out
ghc --make \
  -outputdir out \
  -o out/Spec \
  -threaded ./Spec.hs
  #-hidir out \
  #-odir out \
  #-stubdir out \
  #-dumpdir out \
./out/Spec +RTS -N3
