Small C blog
============

By Sven Michael Klose <pixel@hugbox.org>

# 2024-04-20

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
independent macro assembly.
