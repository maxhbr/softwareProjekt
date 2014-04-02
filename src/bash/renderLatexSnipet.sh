#!/bin/sh

latex -halt-on-error -output-directory /tmp \
 '\documentclass[12pt]{article}' \
 '\pagestyle{empty}' \
 '\begin{document}' \
 '\[' \
 "$@" \
 '\]' \
 '\end{document}' > /dev/null

dvipng -gamma 2 -z 9 -T tight \
  -bg Transparent \
  -o /tmp/snipet.png /tmp/article.dvi > /dev/null

dvipng -gamma 2 -z 9 -T tight \
  -bg White \
  -o /tmp/snipet-w.png /tmp/article.dvi > /dev/null
