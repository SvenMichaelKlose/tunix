Small-C v3.2 Roadmap
====================

See the [TODO file](TODO.xit) about hte
latest ongoings.

Small-C v3.2 will:

1. Get a machine-independent IR code
   generator.  For target specific
   generation a separate tool is made.
2. Run on systems with no more than 39K
   main memory.
3. Run on the Commodore VIC-20 with +35K
   memory expansion.
4. Become self-hosting again.

# Machine-independent IR

There are several code generators that
print assembly code.  The new code
generator will output bytecode, reducing
the size of the compiler and allowing
to externalize final codegeneration,
further reducing the size of the
compiler.

# Run On Low Memory

The tables for symbols, tags, members
and initial values are stored in
btree-indexed database files.  They
are cached record-wise.

# Run On the Commodore VIC-20

This will required a new run-time
library.  That can be generalized for
all Commodore 8-bit machines.

# Become self-hosting

Rewind the latest syntactical updates
but keep it compilable on modern systems
as well.
