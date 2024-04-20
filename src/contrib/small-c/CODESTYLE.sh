#!/bin/sh

# A script to use GNU Indent
#
# Run this script before sending pull
# requests and no-one shall complain.
#
# Please save your changes beforehand
# or there may be tears and grinding
# teeth. ;)

set -e

export FILES=`find . -name \*.[ch]`

# GNU Indent: 4 spaces indentation,
# Kerningham & Ritchie, braces removed,
# 60 chars per line.
indent -v -i4 -kr -br -nprs -l60 $FILES

# Ensure ANSI single-line comments (//).
