Small-C v3.2 Roadmap
====================

See the [TODO file](TODO.xit) about the
latest ongoings.

This derivative will easier to port,
retarget and extendable and be
self-hosting again.

# Used version

The orginal code used is
smallc-v3.0r1.1-unix, as it supports the
most platforms (6809, i80, 68000 and
VAX).

# Enhanced pre-processor

With parametrized macros; probably taken
from small-c-bbc-v0.73.

# Tokenizer

Treating the pre-processed input stream
as tokens simplifies the code as the
compiler can operate on single bytes
instead of strings of variable length.

# Machine-independent IR

In the original version of Small-C there
are several code generators that print
assembly code.  The new code generator
outputs bytecode, reducing the size of
the compiler and allowing to externalize
further processing, leaving more
available memory.

# Run On Low Memory

The tables for symbols, tags, members
and initial values will be stored in
btree-indexed database files.  They
are cached record-wise.  By including
pointers to parents in the tree nodes,
they can be iterated without need for
a return stack.

# Run On the Commodore VIC-20

This will require a minimum run-time
library.  That can be generalized for
all Commodore 8-bit machines.  Old
versions of Small-C have library code
that should be reused.

# Become self-hosting

Cleaning up Small-C, language features
are used which Small-C does not support
and have to be added to avoid overuse
of macros that would spoil the
tangibility of the source code.
Function declarations can be ignored 
(if they aren't already).  The BBC
pre-processor will add macro params.
