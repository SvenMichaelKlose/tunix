AttoLisp
========

A Lisp interpreter for the Commodore
VIC-20 (with UltiMem expansion).

# Overview

This Lisp interpreter knows conses,
symbols/strings and integer numbers.
Garbage collection is not supported
(yet).  It's there mostly for macro
expansions to make human-readable
files machine-readable.

# Built-in functions

* READ
* PRINT
* GC
* .
* CAR
* CDR
* LIST
* =
* ?
* PROGN
* BLOCK, GO
* DUMP
* APPLY
* &
* |
* EQ
* NOT
* SYS
* PEEKB, PEEKW
* POKEB, POKEW
* ==, <, >
* +. -. *. /, %
