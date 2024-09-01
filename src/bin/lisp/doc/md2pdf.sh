#!/bin/sh

pandoc -V documentclass=report --from markdown --template eisvogel.tex --listings -o manual.pdf manual.md
pandoc -V documentclass=report --from markdown --template eisvogel.tex --listings -o compiler.pdf compiler.md
