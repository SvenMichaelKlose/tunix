Small C blog
============

Informal narratives about the project.

# 2024-04-22 22:52 S.M. Klose

I've gathered some more version.  Please
let me know if you know more and where
to get them.  They might turn out to be
a rich source for improvements and new
ideas.

smallc-v3.0r1.1-6809,68000,8080,vax is
the one I started out with.  Good idea
as it's multi-platform and I want it to
stay that way.  Might one day reuse the
targets.

smallc-plus-v1.06a-TMS9900-99000 &
smallc-plus-v1.0-z80-cpm know doubles
and floats.  Oh yeah!  Not the most
urging issue though. Target reuse might
happen here as well.

To "just"
port smallc-bbc-v0.73-6502-selfhosting
wouldn't work for the VIC (the ROM was
also used on the BBC) and as I said I
want the compiler to be as flexible as
possible. You never know... the
apocalypse is nearing... when you're
desperate for compiling VAX code with
only your VIC left. ;)

smallc-byte-v1.0-8086-dos,
smallc-v1.0-8086-cpm &
smallc-v1.2-8086-dos-to-cpm also have
targets for reuse.

As a side thought: will use parametrized
macros anyhow as we're cross-compiling.

# 2024-04-22 14:23 S.M. Klose

Oh dude.  Small-C has quite some
heritage tree.  Makes it hard to apply
a version numbers.

Groepaz (think VICE emulator) pointed me
to the self-hosting BBC version of
Small-C.  It has very, very useful
additions that are dearly missing in our
version:

* Separate preprocessor: that's what any
  8-bit version of Small-C requires, due
  to memory constraints.
* Parametrized macros: Macros with
  arguments.  The classic tool to
  improve readability without
  sacrificing performance (as they come
  with "inlining" by definition).
* Hash tables for symbol storage:
  A classic.  And one of those things
  that shouldn't be there more than once
  but it is.  The VIC version will use
  "database" files.  I was thinking more
  of b-tree indexes as they are more
  all-purpose.  Won't work with a drive
  that DOS not support random read/write
  access, no matter what kind of index
  is used.
* Forward declarations and typed return
  values: That's huge.

Doabouts:

* Using the BBC preprocessor instead
  *should* be no problem.  The new
  macro capabilities will lead to
  another grand clean-up, changing the
  face of the project.
* Database files: I'll go for b-trees
  just in case the VIC needs a database.
  Wanted a lib for that a couple of
  times already.
* Type-checking: Yo. Nerd to see what
  things look like when cleaned up with
  macros.

I'll put all versions of Small-C I have
laying around into a single repository
for preservation.  But as usual: no pro
work without research.

# 2024-04-22 00:02 S.M. Klose

Generating IR codes has been updated big time and 'ir2txt'
generates first correct IR assembly that'll go through
6502/ca65-macros.asm next.  And the first automated tests.

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
