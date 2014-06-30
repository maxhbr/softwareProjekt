#!/bin/bash

if [[ $# != 0 ]]; then
  if [[ $1 == "-N" ]]; then
    shift
    N=$1
    shift
  fi
else
  N=1
fi

function myawk() {
  awk 'BEGIN {FS=","} $1 ~ /.*'$1'/ {match($1, ".*@ ([0-9]+)", arr); print arr[1],$2}' bench_out/benchMult_${STAMP}.csv
}
function myawk2() {
  awk 'BEGIN {FS=","} $1 ~ /.*'$1'/ {match($1, ".*@ ([0-9]+)", arr); print $2}' bench_out/benchMult_${STAMP}.csv
}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
mkdir -p ${DIR}/bench_out
STAMP=$( date +%Y-%m-%d_%H:%M:%S )

pushd $DIR
cabal configure --enable-benchmarks \
  && cabal build \
  && cabal bench benchMult --benchmark-options="-u bench_out/benchMult_${STAMP}.csv"
echo -e "deg\tNorm\tKarat\tFFT" >bench_out/benchMult_${STAMP}.dat
paste <(myawk Norm) <(paste <(myawk2 Kar) <(myawk2 FFT))  >>bench_out/benchMult_${STAMP}.dat
gnuplot <<EOF
set key below

set term push
set term post enh color lw 1 12 "Times-Roman"
set output "bench_out/benchMult_${STAMP}.eps"
plot for [col=2:4] "bench_out/benchMult_${STAMP}.dat" using 1:col title columnheader with lines
EOF
popd
