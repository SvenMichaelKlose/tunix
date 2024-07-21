The TUNIX Project
=================

This project attempts to create an operating system for
minimalistic computers that is portable, self-hosting, and
multi-tasking, complete with a comprehensive set of
applications.

* [Build](BUILD.md)
* [Discuss](https://github.com/SvenMichaelKlose/tunix/discussions)

To be self-hosting, the plan is to have a C compiler written
in Lisp (for which a compiler also exists), as Lisp is more
powerful and expressive than C.  The C programming language,
however, is far more popular than Lisp, connecting the
system to more developers and existing applications.

* The Lisp interpreter is written in C.
* The Lisp compiler is written in itself, producing bytecode
  and native assembly.
* The C compiler and assembler are written in Lisp.

To start, the Lisp interpreter is compiled using the cc65
compiler suite[^cc65], targeting classic home computers.
Memory expansions, disk drives, and SD-card drives are
readily available for these machines.  This secondary
storage makes the TUNIX project possible.

[^cc65]: [cc65 Git repository](https://github.com/cc65/cc65)

Multi-tasking is achieved by the TUNIX kernel when enough
banked memory is available to switch between processes
several times per second.  Otherwise, only the Lisp can
provide similar functionality.

On the Commodore VIC-20, an UltiMem expansion[^ultimem],
manufactured by Jim Brain's company Retro Innovations, and
an SD2IEC drive provide everything TUNIX needs.  Since the
initiator of this project owns such a system, TUNIX will
likely be completed on it first.

# Anything Interesting for Non-Developers?

Small systems like TUNIX have great educational value since
individuals can understand them fully, especially if
business logic is not a factor.  By combining simple,
well-known techniques, a practical tool for everyday use can
be built, and unused hardware can be revived.

# State of Affairs

A kernel for the Commodore VIC-20 with Ultimem expansion was
almost complete but has been dropped because it was written
in assembly, which is difficult to maintain.  It is not
portable either.  A few lines of C would have sufficed, as
seen in the C kernel sketch alongside the assembly version.

The Lisp interpreter is currently the project's focus.  See
'src/bin/lisp/' and 'src/lib/lisp/'.  It runs on the
Commodore C128, C16, C64, Plus/4, VIC-20, and any standard
Unix-like system, such as Linux or BSDs with a
GNU-compatible C compiler toolchain.

There is also a set of other applications, mostly running on
the VIC-20 with UltiMem expansion, including a GUI and a
tiny VI editor clone, all written in C.

Outdated items can be found in 'attic' directories.
'playground' directories may contain anything interesting,
even if it looks like nonsense, and 'growroom' directories
hold things intended to get up and running but are not
functional yet.

Guidelines and roadmaps aren't definite and are open to
discussion.  Feel free to provide your perspective, even if
you don't wish to discuss it at length.
