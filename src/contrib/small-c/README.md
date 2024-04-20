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

# Operators

## Arithmetic Operators

### Addition (`+`)

Adds two operands.

~~~C
a = 5;
b = 3;
c = a + b; // c is 8
~~~

### Subtraction (`-`)

Subtracts the second operand from the
first.

~~~C
a = 5;
b = 3;
c = a - b; // c is 2
~~~

### Multiplication (`*`)

Multiplies two operands.

~~~C
a = 5;
b = 3;
c = a * b; // c is 15
~~~

### Division (`/`)**

Divides the first operand by the second.

~~~C
a = 6;
b = 3;
c = a / b; // c is 2
~~~

### Modulo (`%`)

Returns the remainder of division of
the first operand by the second.

~~~C
a = 5;
b = 3;
c = a % b; // c is 2
~~~

## Relational Operators

### Equal to (`==`)

Checks if two operands are equal.

~~~C
if (a == b) { ... }
~~~

### Not equal to (`!=`)

Checks if two operands are not equal.

~~~C
if (a != b) { ... }
~~~

### Greater than (`>`)

Checks if the first operand is greater
than the second.

~~~C
if (a > b) { ... }
~~~

### Less than (`<`)

Checks if the first operand is less
than the second.

~~~C
if (a < b) { ... }
~~~

### Greater than or equal to (`>=`)

Checks if the first operand is greater
than or equal to the second.

~~~C
if (a >= b) { ... }
~~~

### Less than or equal to (`<=`)

Checks if the first operand is less
than or equal to the second.

~~~C
if (a <= b) { ... }
~~~

# Logical Operators

### Logical AND (`&&`)

Returns true if both statements are
true.

~~~C
if (a > 10 && b < 5) { ... }
~~~

### Logical OR (`||`)

Returns true if at least one statement
is true.

~~~C
if (a > 10 || b < 5) { ... }
~~~

### Logical NOT (`!`)

Reverses the logical state of its
operand.

~~~C
if (!a) { ... }
~~~

## Bitwise Operators

### AND (`&`)

Applies the AND operation on each pair of bits.

~~~C
c = a & b; // bitwise AND
~~~

### Inclusive OR (`|`)

Applies the OR operation on each pair of
bits.

~~~C
c = a | b; // bitwise OR
~~~

### Exclusive OR (`^`)

Applies the XOR operation on each pair
of bits.

~~~C
c = a ^ b; // bitwise XOR
~~~

### Complement (`~`)

Flips all the bits.

~~~C
c = ~a; // bitwise NOT
~~~

### Left Shift (`<<`)

Shifts bits to the left, fills 0 on the
voids left as a result.

~~~C
c = a << 2; // left shift by 2
~~~

### Right Shift (`>>`)

Shifts bits to the right, fills 0 on the
voids left as a result.

~~~C
c = a >> 2; // right shift by 2
~~~

## Assignment Operators

### Assignment (`=`)

Assigns values from right side operands
to left side operand.

~~~C
int a = 5;
~~~

### Add AND (`+=`)

It adds the right operand to the left
operand and assign the result to the
left operand.

~~~C
a += b; // equivalent to a = a + b
~~~

### Subtract AND (`-=`)

It subtracts the right operand from the
left operand and assigns the result to
the left operand.

~~~C
a -= b; // equivalent to a = a - b
~~~

### Multiply AND (`*=`)

It multiplies the right operand with the
left operand and assigns the result to
the left operand.

~~~C
a *= b; // equivalent to a = a * b
~~~

### Divide (`/=`)

It divides the left operand with the
right operand and assigns the result to
the left operand.

~~~C
a /= b; // equivalent to a = a / b
~~~

### Modulo (`%=`)

Takes the modulus using the two operands
and assigns the result to the left
operand.

~~~C
a %= b; // equivalent to a = a % b
~~~

## Special Assignment Operators

### Bitwise AND assignment (&=)

Applies bitwise AND operation and
assigns the result to the left operand.

~~~C
c &= 2; // equivalent to c = c & 2
~~~

### Bitwise OR assignment (|=)

Applies bitwise OR operation and assigns
the result to the left operand.

~~~C
c |= 2; // equivalent to c = c | 2
~~~

### Bitwise XOR assignment (^=)

Applies bitwise XOR operation and
assigns the result to the left operand.

~~~C
c ^= 2; // equivalent to c = c ^ 2
~~~

### Left Shift assignment (<<=)

Shifts the left operand's bits to the
left, assigns the result to the left
operand.

~~~C
c <<= 2; // equivalent to c = c << 2
~~~

### Right Shift assignment (>>=)

Shifts the left operand's bits to the
right, assigns the result to the left
operand.

~~~C
c >>= 2; // equivalent to c = c >> 2
~~~

### Comma Operator

## Comma (,)

Allows two expressions to be evaluated
in the same statement with the result of
the last expression being the value of
the combined expression.

~~~C
// b is set to 3, and then a is set to 5
a = (b = 3, b + 2);
~~~

## Increment and Decrement Operators

### Increment (++)

Increases an integer value by one.

~~~C
++a; // pre-increment
a++; // post-increment

### Decrement (--)

Decreases an integer value by one.

~~~C
--a; // pre-decrement
a--; // post-decrement
~~~

## Conditional Operator

### Conditional (?:)

Returns one of two values based on the
condition given.

~~~C
// result is 'a' if a > b, otherwise 'b'
int result = (a > b) ? a : b;
~~~

## Type operators

### Cast

Converts the value of one data type to
another.

~~~C
int b = (int) a; // casts 'a' to int.
~~~

### sizeof()

Returns the size of a type in bytes.

~~~C
i = sizeof (char);  // 1
i = sizeof (int);   // 2
i = sizeof (struct ure); // Depends,,,
~~~

## Pointer And Member Access Operators

### Address of (`&`)

Returns the memory address of its
operand.

~~~C
int a = 10;
int* ptr;
ptr = &a; // ptr holds the address of a
~~~

### Dereference (`*`)

Accesses the value at the address
pointed to by its operand (pointer).

~~~C
int a = 10;
int* ptr;
int b;
ptr = &a;
b = *ptr; // b is 10.
~~~

### Arrow (->)

Accesses a member of a structure or
union through a pointer.

~~~C
struct Point {int x; int y;};
struct Point *p;
*p = &point;
int x;
x = p->x; // Access x via pointer
~~~

## Array Operators

### Subscription (`[]`)

Accesses an element at an index in an
array.

~~~C
int arr[3] = {10, 20, 30};
int x;
x = arr[1]; // x is 20
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
