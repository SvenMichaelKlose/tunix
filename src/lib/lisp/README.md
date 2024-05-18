Lisp heap & interpreter
=======================

# SYNOPSIS

\#include <lisp/liblisp.h>

# DESCRIPTION

`liblisp` is a Lisp library.  It provides basic Lisp
data structures and operations in C that would come across
as READ and PRINT function is Lisp.  Additionally, Lisp
code can be interpreted using user-defined built-in
functions.

# DATA TYPES

Conses, long integer numbers and symbols are supported.
Please see 'liblisp.h' for the wildly changing details.

# GARBAGE COLLECTION

A compacting mark-and-sweep GC is used, so the heap can
be loaded and saved in one block.  The sweep phase can take
multiple turns if there is little space left for relocation
info.

# MAYBE: LIST COMPRESSION

A new cons linked to the previously created one is
compressed to take only three bytes instead of five.  Its
CDR is immutable.  This is very effective with recursive
list assembly.
A compressing GC sweep may also be an option.

# SEE ALSO

https://en.wikipedia.org/wiki/Lisp_(programming\_language)

# AUTHORS

Developed by Sven Michael Klose <pixel@hugbox.org> as part
of the TUNIX project.

# COPYRIGHT

This software is in the public domain and you can do with it
as you please.
