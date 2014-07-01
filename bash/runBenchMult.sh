#!/bin/bash

function myawk() {
  awk 'BEGIN {FS=","} $1 ~ /.*'$1'/ {match($1, ".*@ ([0-9]+)", arr); print arr[1],$2}' \
    ${DIR}/dist/bench_out/benchMult_${STAMP}.csv
}
function myawk2() {
  awk 'BEGIN {FS=","} $1 ~ /.*'$1'/ {match($1, ".*@ ([0-9]+)", arr); print $2}' \
    ${DIR}/dist/bench_out/benchMult_${STAMP}.csv
}
function getField() {
  awk 'BEGIN {FS=" "} NR==2 {print $2}' \
    ${DIR}/dist/bench_out/benchMult_${STAMP}.csv
}

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )/.."
mkdir -p ${DIR}/dist/bench_out
STAMP=$( date +%Y-%m-%d_%H:%M:%S )

pushd $DIR
#cabal configure --enable-benchmarks \
  #&& cabal build -j \
  #&& cabal bench benchMult \
    #--benchmark-options="-u ${DIR}/dist/bench_out/benchMult_${STAMP}.csv"
STAMP=2014-06-30_17:57:31

echo -e "deg\tNorm\tKarat" >${DIR}/dist/bench_out/benchMult_${STAMP}.dat
paste <(myawk Norm) <(myawk2 Kar) \
  >>${DIR}/dist/bench_out/benchMult_${STAMP}.dat

gnuplot <<EOF
set samples 1001
set key below

set term push
set term post enh color lw 1 12 "Times-Roman"

set logscale y 2

set title "Vergleich Multiplikationsalgorithmen ueber $( getField )"

set output "${DIR}/dist/bench_out/benchMult_${STAMP}.eps"
plot for [col=2:4] "${DIR}/dist/bench_out/benchMult_${STAMP}.dat" \
  using 1:col title columnheader with lines
EOF

epstopdf "${DIR}/dist/bench_out/benchMult_${STAMP}.eps" \
  --outfile "${DIR}/dist/bench_out/benchMult_${STAMP}.pdf"

popd
