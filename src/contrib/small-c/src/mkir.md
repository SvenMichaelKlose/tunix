mkir – Generate IR bytecode C header
====================================

# Synopsis

~~~
mkir
~~~

# Usage

Prints a '#define' statement for each
code in "src/ir-table-h" to assign
enumerated integers which are greater
than 0.
IR code names are prefixed with "IR_",
ie. "LDAMC" becomes "IR_LDAMC" in C:

~~~C
#define LDAMC   <some integer>
~~~

# See also

* [ir2txt](ir2txt.md)
