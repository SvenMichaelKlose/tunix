Small C Version 3.2
===================

# Overview

Small C is an enhanced version of the
public domain compiler for a subset of
the C programming language, based on
Chris Lewis' revival of Ron Cain's
original Small C compiler.

Chris Lewis introduced several upgrades
and new features while maintaining the
simplicity and fun of tinkering that the
earlier versions promoted.

P.L. Woods added ANSI-style argument
type declarations.

More cleaning up was done by
https://github.com/ncb85/SmallC-857
which was used as the basis for what
you have here.

# Status

This ersion intents to add
documentation, a code generator for the
MOS 6502-CPU and to compile Small-C by
itself on such systems.  (Older versions
were able to compile themselves.)

# New in v3.1

For 'unsigned int' declarations the
'int' keyword can be omitted:

~~~C
unsigned int i;
unsigned i2;    // Same.
~~~

# Language

Comment may be K&R or ANSI-style.

~~~C
/*
  Kerningham & Ritchie style comment
*/

// ANSI one-liner
~~~

Small-C supports signed or unsigned
chars and ints, as well as structs
and unions.  Shorts are the same as
ints.  Variacle declarations cannot
have initializers.

~~~C
char  c;
char  c = 0;    // No.
short s;
unsinged u;     // unsinged int
int      i;
unsigned char  uc;
unsigned short us;
unsigned int   ii;
struc btnode {
    char parent;
    char left;
    char right;
    char data;
};
union {
    char same;
    char memory;
    char location;
    char for_all_four;
};
~~~

Pointers are supported with single
indirection.

~~~C
    return *ptrs;
    return **ptrs;  // No.
    return rec->entry;
    return res->entry->next;  // No.
~~~

Arrays can only have one dimension.

~~~C
char array1[5];
char array1[23][42]; // No.
~~~

Initializers must be constant.

~~~C
char i = 2;
char k = 1 + 1;     // No.
char array[4];
char array[4 + 1];  // No,
char array[4] = {1, 2, 3, 4};
char array[4] = {1, 2 + 3, 4}; // No.
struct node {
    char left;
    char right;
} = {1, 2};
~~~

Eventually variables want to be exported
for the linker:

~~~C
extern char global_state;
~~~

Functions must not be declared and their
argument definitions are not known or
checked at call time.  The return value
may be 'void' or omitted (which defaults
to int but the return type cannot be
checked at compile-time).

Local variables have to precede the
statements.  They can be declared
static.

~~~C

some_fun(); // No forward declarations!

// K&R-style argument types.
some_fun (to, have)
char *to;
int  have;
{
    int i;
    static int j;
}

// ANSI-style argument types.
some_fun (char *to, int have)
{
}
~~~

Control flow expressopms are: do, for,
if, switch/case, while and return.

Literals are:

~~~
'c'
"string"
23          // decimal
0x1234      // hexadecimal
0XDEADBEEF  // hexadecimal
0755        // octal
~~~

Operators are:

~~~
// Unary
!, - ++, --, ~
*, [], ->, &

Binary
+, -, *, /, %, ^
==, !=, <, >, <=, >=
|, &,
||, &&,
<<, >>

// Assignment
=, +=, -=, *=, /=, %=
|=, &=, %=, ^=
<<=, >>=

sizeof()
~~~

# Supported Targets

Currently the MOS 6502 target is under
construction.

# Compilation and Usage

Use the provided Makefile appropriate
for your system (`System V` or `BSD`).
Warnings by the host compiler are
pretty normal.

# Adding New Targets

Adding support for new machine targets
involves creating a new `codexxx.c` file
for the target without altering existing
infrastructure.

# Contrinuting

Contributions for new coders or bug
fixes are highly welcomed; please
contact the maintainers via GitHub.
