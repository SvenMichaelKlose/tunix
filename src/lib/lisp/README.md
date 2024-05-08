Lisp heap
=========

# SYNOPSIS

\#include <lisp/liblisp.h>

# DESCRIPTION

`liblisp` is a Lisp heap library.  It provides basic Lisp
data structures and operations in C that would come across
as READ and PRINT function is Lisp.

The heap has no garbace collection and is growing upwards.

# DATA TYPES

Conses, long integer numbers and symbols are supported.
For each tye a C data structure is declared.
See 'liblisp.h'.

# MACROS

* `PTRTYPE(x)`: Retrieves the type of the Lisp object `x`.
* `CONSP(x)`: Checks if `x` is a cons cell.
* `NOTP(x)`: Checks if `x` is NIL.

# FUNCTIONS

* `lispptr lisp_read (void)`:
  Reads expression from the terminal.
* `lispptr lisp_print (lispptr)`:
  Prints expression to the terminal.
* `lispptr make_cons (lispptr car, lispptr cdr)`:
  Creates a new cons cell with `car` and `cdr` values.

  ```c
  lispptr my_cons = make_cons (my_car, my_cdr);
  ```
* `lispptr make_number (long value)`:
  Creates a numeric Lisp object with the specified `value`.

  ```c
  lispptr num = make_number (42);
  ```
* `lispptr make_symbol (const char *name)`:
  Creates a symbol with the specified `name`.

  ```c
  lispptr sym = make_symbol ("example");
  ```

# EXAMPLES

```c
#include <liblisp.h>

void
main ()
{
    lisp_print (lisp_read ());
}
```

# SEE ALSO

https://en.wikipedia.org/wiki/Lisp_(programming\_language)

# AUTHORS

Developed by Sven Michael Klose <pixel@hugbox.org> as part
of the TUNIX project.

# COPYRIGHT

This software is in the public domain and you can do with it
as you please.
