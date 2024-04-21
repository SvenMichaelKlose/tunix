Small C blog
============

Informal narratives about the project.

# 2024-04-21 11:24 S.M. Klose

New 'mkir' is generating the 'ir.h'
file.  Goes well with the missing 'enum'
of the Small-C dialect.  Four hours of
sleep wasn't a big deal.

Started writing ca65 assembly macros to
gain more insight.

# 2024-04-21 06:48 S.M. Klose

The code generator is now producing
untested IR.  The output file is one
20th of the size of what the assembly
files have been.  Maybe a bytecode
interpreter will save the day later,
when this runs out of address space.

It's now time to create an IR to
assembly converter, a fitting macro set
for ca65, and to set up the testing.
I'm not sure what is going on with the
two registers and the top of the stack
for sure.  More documentation will come
out of that first.

# 2024-04-21 01:34 S.M. Klose

The first jobs have been finished to
make the source accessible better.
Most of the original sources have been
cleaned up and documentation has been
created alongside.  As the sources have
to be readable on the target platform,
a Commodore VIC-20, it has all been
formatted for 40 column displays.  Parts
of the source code with exaggerated
line widths should be cleaned up some
time, but at the moment it is essential
to not break much until automated tests
are up and running.

Instead of implementing a 6502-CPU code
generator right away, the new one will
emit a machine-independent bytecode
which may be converted into machine-
independent macro assembly.  It is the
exact order of instructions like before.
