#!/bin/sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo $DIR
\cp "${DIR}/.lualatexmk_files/main.pdf" "${DIR}/../pdf/output.pdf"
git commit -m "update PDF" "${DIR}/../pdf/output.pdf"
