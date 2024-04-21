scc - Small-C compiler frontend
===============================

# Uusage

~~~sh
scc [-tah] [-dSYM[=VALUE]] [-l[log]] input-files
~~~

* input-files: Use standard I/O if
  missing.  They have to end on suffix
  '.c', which whill be changed to '.o'
  for the output object file.
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
