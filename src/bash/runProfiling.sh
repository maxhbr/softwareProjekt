#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/../"
mkdir -p ${DIR}out
mkdir -p ${DIR}out/Profiling

ghc \
  -outputdir ${DIR}out/Profiling \
  -o ${DIR}out/Profiling/AlgProfiling \
  -fforce-recomp \
  -prof \
  -fprof-auto -rtsopts \
    ${DIR}AlgProfiling.hs

if [ $? -eq 0 ]; then
  ${DIR}out/Profiling/AlgProfiling +RTS -p
fi

# -fforce-recomp, -O2 and -fllvm are from:
  #-threaded \
  #-O2 \
  #-fllvm \
