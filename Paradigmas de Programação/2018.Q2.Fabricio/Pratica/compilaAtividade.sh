#!/bin/bash
gpp -H $1.md | pandoc --mathjax -f markdown+emoji --pdf-engine=xelatex --highlight-style=pygments -o $1.pdf
