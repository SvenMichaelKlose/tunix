#!/bin/sh

pandoc --from markdown --template eisvogel.tex --listings --top-level-division="chapter" index.md -o index.pdf
