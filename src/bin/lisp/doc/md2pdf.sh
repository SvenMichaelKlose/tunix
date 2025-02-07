#!/bin/sh

pandoc -V documentclass=report --from markdown --template eisvogel.tex --listings --pdf-engine=xelatex -V mainfont="Noto Sans" -o manual.pdf manual.md
pandoc -V documentclass=report --from markdown --template eisvogel.tex --listings --pdf-engine=xelatex -V mainfont="Noto Sans" -V documentclass=report -o compiler.pdf compiler.md
