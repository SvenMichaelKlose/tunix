#!/bin/sh

set -e

export FILES=`find . -name \*.[ch]`

# 4 spaces indent, Kerningham & Ritchie,
# braces removed, 40 chars wide max.
indent -v -i4 -kr -br -nprs -l60 $FILES

# Use '//' for single-line comments.
sed -i 's|/\*\(.*\)\*/|//\1|' $FILES
