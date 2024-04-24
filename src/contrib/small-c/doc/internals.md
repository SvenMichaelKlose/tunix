Small C Internals
=================

Small-C is a one-pass compiler which
does preprocessing, parsing and
compiling in one go.  It generates code
as soon as possible, while travelling
along the tree of expressions that
stream in character by character as
source code.  It translates these
expression into a one or more virtual
machine instructions which are later
translated into machine-specific code:
As the name suggest, that virtual
machine does not exist.  It is idealized
so the compiler can perform its function
with ease, using only two 16-bit
registers (called the 'primary' and the
'secondary' register) and the stack: to
translate expressions into a sequence of
most-basic instructions only assembly
language programmers would care about.

These instructions are output as "byte
codes", each the size a byte, with an
optional word or ASCIIZ string following
as argument, depending on the
instruction.  Then those byte codes are
turned into lines of text and
translated into machine code using a
assembler, covering all machine-specific
details the compiler does not have to
care about any more.  That brings
several advantages: the compiler is
easier to maintain and can be ported to
other architectures swiftly.

# 6502-CPU specifics

No C stack is added, so all stack space
is 256 bytes in total.
