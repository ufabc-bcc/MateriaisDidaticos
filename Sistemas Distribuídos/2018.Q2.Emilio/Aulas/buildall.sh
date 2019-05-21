#!/bin/sh
mkdir -p build
rsync -av . ./build --exclude build
cd build
# -outputdir wreaks havoc with minted
latexmk -shell-escape -pdf -xelatex *.tex
cp *.pdf ..
