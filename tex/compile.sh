#!/bin/sh
/usr/bin/latexmk -lualatex --shell-escape -synctex=1 -pvc \
  -outdir=/tmp/latexmk_files/ -auxdir=/tmp/latexmk_files/ \
  main.tex

