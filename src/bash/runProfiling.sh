#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../"
mkdir -p ${DIR}outP

ghc \
  -outputdir ${DIR}outP \
  -o ${DIR}outP/AlgProfiling \
  -fforce-recomp \
  -prof \
  -fprof-auto -rtsopts \
    ${DIR}AlgProfiling.hs

if [ $? -eq 0 ]; then
  ${DIR}outP/AlgProfiling +RTS -p
fi

# -fforce-recomp, -O2 and -fllvm are from:
  #-threaded \
  #-O2 \
  #-fllvm \
