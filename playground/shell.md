TUNIX shell
===========

# Requirements

* minimum prerequisites required to (get to) run

# Purpose

* user-friendly way of executing commands
* taking off repetitive tasks through automation
  ("scripting")
* provide a full-blown programming language for application
  development

# Doabouts

The Lisp interpreter into a user-friendly
command-line tool by adding an impertive input mode: Lines
not starting with a parenthesis are treated like regular
program invokation lines, starting with a programm name,
followed by optional arguments.

~~~sh
cat /etc/release
~~~

Within normal Lisp code, the SYSTEM function invokes
program.  This time, symbols are being evaluated, so they
either need to be quoted to prevent evaluation or notated
as literal strings.  ^ is the shortcut for SYSTEM, like
' is for QUOTE.

~~~sh
(princ (system "cat" "/etc/release"))
(princ ^("cat" "/etc/release"))
(princ ^('cat '/etc/release))
~~~

Two things SYSTEM returns are of interest: the exit code of
the executed program and its standard output (and standard
error output).  By default SYSTEM returns the programs
output as a string if the exit code is 0.  It returns the
exit code as a number otherwise.

~~~sh
(@ print (lines ^('cat '/etc/release)))
~~~
