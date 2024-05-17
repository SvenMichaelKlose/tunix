TUNIX Lisp interpreter
======================

This is an extensible interpreter with mark-and-sweep
garbage collection.

The interpreter knows these data types:

* number (16 bit)
* cons
* symbol (0-255 char name)
* string (1-256 chars)
* array (256 objects max.)
* builtin

Functions are conses with the CAR holding the argument
definition and the CDR holding the body.

# Futue extensions

Numbers from 1 to 255 are pointer-encoded and do not
require garbage collection.

# Tail of call stack swapped to disk
