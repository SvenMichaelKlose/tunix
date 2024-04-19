Small C Version 3.2
===================

# Overview

Small C is an enhanced version of the
public domain compiler for a subset of
the C programming language, based on
Chris Lewis' revival of Ron Cain's
original Small C compiler.  Chris Lewis
introduced several upgrades and new
features while maintaining the
simplicity and fun of tinkering that the
earlier versions promoted.  It has then
been cleaned up by
https://github.com/ncb85/SmallC-857
from where this version has been forked.

# Statis

This ersion intents to add a code
generator for the MOS 6502-CPU and to
compile scc by itself on such systems.

# Features

* **Basic Language Support**: While
  lacking complex features like `#if`
  preprocessor directives and support
  for `double`, `float`, and `long` data
  types, Small C v3.1 can compile a
  subset of C sufficient to compile
  itself and other simple programs.
* **Structures and Unions**: Introduced
  handling for structs and unions which
  were not supported in previous
  releases.
* **Unsigned Types & C99 One Line
  Comments**: Supports unsigned integers
  and one-line comments (`//`) as per
  the C99 standard.
* **Windows EOLs**: Adds the capability
  to handle Windows-style end of lines,
  enhancing cross-platform usability.
* **Global Variables Initialization**:
  Global variables are automatically
  initialized to zero if not explicitly
  initialized by the programmer.
* **Advanced Instruction Set Support**:
  Includes undocumented 8085
  instructions like LHLX, SHLX, LDSI,
  ARHL for better control over
  hardware-specific operations.
* **Enhanced Method Declarations**:
  Supports ANSI style function and
  method declarations.
* **Silent Compilation with GCC**:
  Rewritten parts of the codebase to
  ensure silent compilation with GCC,
  avoiding warnings.
* **Target Assemblers**: Generates code
  suitable for the ASXXXX
  assembler/linker system.

# Supported Targets

* **Code Generators**: Initially
  supports a variety of target
  architectures including 6809, M68K,
  VAX, and 8080 with provided Makefiles
  for System V and BSD compatible
  systems.
* **Runtime Support**: The generated
  code may require a runtime support
  library for operations difficult to
  process on certain CPUs or for OS
  interface functions (specifics vary by
  target processor).

# Compilation and Usage

* **Compiling Small C**: Use the
  provided Makefile appropriate for your
  system (`System V` or `BSD`).  Adjust
  `INCDIR` and `LIBDIR` in the Makefile
  to specify custom locations for the
  compiler output.
* **Warnings**: Due to its subset
  nature, compiling Small C with
  standard UNIX C compilers will
  generate multiple warnings which can
  be safely ignored as long as pointers
  and integers are treated
  interchangeably and have the same
  size.

# Adding New Targets

* **Custom Code Generators**: Adding
  support for new machine targets
  involves creating a new `codexxx.c`
  file for the target without altering
  existing infrastructure.
* **Support and Contributions**:
  Contributions for new coders or bug
  fixes are highly welcomed; please
  contact the maintainers if you
  encounter any issues or have
  enhancements.

# Contact

For contributions, bug reports, or
additional support, please contact the
maintainers via GitHub.
