Small C Internals
=================

Small-C is a one-pass compiler which
does preprocessing, parsing and
compiling in one go.  It generates code
as soon as possible, while travelling
along the tree of statements and
expressions that stream in character by
character as source code.  It translates
each expression into a one or more
virtual machine instructions.  As the
name suggest, that machine does not
exist, but it allow the compiler to use
only two 16-bit registers (called
'primary' and 'secondary' register) and
the stack as a general model to convert
expressions into sequences of
instructions.  These instructions are
output as byte codes with an optional
word or ASCIIZ string argument
following, depending on the instruction.
Then those byte codes can then be
further processed or turned into lines
of text using 'ir2txt' and be translated
to assembly language using
target-specific macros.  The final
assembly then generates the desired
executable.

Apart from the macros for the target
CPU, a run-time library is also required
to run a program.  To add a new target,
these two things have to be provided.
That may include adding a new assembler
for the target CPU.

Which IR byte codes need to be handle
by every target is defined in the common
[IR table](../src/ir-table.c).

# 6502-CPU specifics

Local variables (and function arguments
alongside) are stored on the CPU stack.
All arguments are of size 'int'.
