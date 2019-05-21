#!/bin/bash
pandoc -t beamer --highlight-style=pygments --template=template.tex --pdf-engine=xelatex -o $1.pdf $1.md
#pandoc -t beamer -s --mathjax -f markdown+emoji+grid_tables --pdf-engine=xelatex -V theme:metropolis --highlight-style=pygments -o $1.pdf --variable mainfont="DejaVu Sans" $1.md
