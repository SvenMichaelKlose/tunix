ir2txt – IR byte code to text converter
=======================================

# Synopsis

~~~
ir2txt
~~~

# Usage

Reads IR byte codes from stdin and
translates each one into a line of
text, containing the name of the code
in lower case and prefixed by 'v',
followed by a decimal integer or a
string ending with the line.

~~~C
vdefglobal some_fun
vpusha
vldamc 5
vaddsp
vpopb
vadd
~~~

These lines can be further processed by
macro assemblers that can handle these
lines with a dedicated set of macros.

# See also

* [mkir](mkir.md)
