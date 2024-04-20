Small C Targets (v3.2)
======================

# Adding New Targets

Adding support for new targets merely
involves creating a new code generator
from an existing one to print code for
an assembler (or whatever else) of your
choice.

## Code Generator Predefinitions

Define ASNM and LDNM to the names of
the assembler and linker respectively

INTSIZE is the size of an integer in
the target machine.
BYTEOFF is the offset of an byte within
an integer on the target machine. (ie:
8080,pdp11 = 0, 6809 = 1, 360 = 3)

## Compiler Architecture

Small-C is a one-pass compiler which
does preprocessing, parsing and
compiling in one go.

Internally the intermediate
representation is a two-adress machine
using two int registers instead of
addresses.  One register is called the
'primary register' as it always holds
the first operand and the result of an
operation.  The 'secondary' register
serves holding an optional second
operand or as temporary storage for
intermediate results.

Local variables (and function arguments
alongside) are stored on the CPU stack.
All arguments are of size 'int'.

Code generators need to provide 15
well-defined functions to be complete.
