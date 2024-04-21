scc - Small-C compiler frontend
===============================

# Uusage

~~~sh
scc [-tah] [-dSYM[=VALUE]] input-files
~~~

Compiles input-files one by one and
and generates object code.  Standard I/O
is used instead, if missing.

Specified input file names have to end
on suffix '.c', which whill be changed
to '.o' for the output object file.

* -h: Print help text.
* -t: Include source in object files.
* -dSYM[=VALUE]:
  Define macro.  Default value is '1'.

# Authors

* Ron Cain
* Chris Lewis
* P.L. Woods
* S.M. Klose <pixel@hugbox.org>

# See Also:

[Github repository](https://github.com/SvenMichaelKlose/small-c/)

# Copyright

This software is in the Public Domain.
