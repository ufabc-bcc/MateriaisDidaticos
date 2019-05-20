#!/bin/bash
gpp -H $1.md | pandoc -t beamer -s --mathjax -f markdown+emoji --pdf-engine=xelatex -V theme:metropolis --highlight-style=pygments -o $1.pdf --variable mainfont="DejaVu Sans"
