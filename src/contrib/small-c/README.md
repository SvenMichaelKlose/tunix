Small C Version 3.2
===================

# Overview

This is a compiler for a subset of
the C programming language.

## Intent

This ersion intents to add documentation
and self-hosting on 6502-CPU platforms.

# Compilation And Usage

Use the provided Makefile appropriate
for your system (`System V` or `BSD`).
Warnings by the host compiler are
pretty normal.

If you intend to add other machine
pleas see
[Adding New Targets Oneself](#adding_targets).

# Contributions And Contributors

This version is based on Chris Lewis'
revival of Ron Cain's original Small-C
compiler.  He introduced several
upgrades and new features while
maintaining the simplicity and fun of
tinkering that the earlier versions
promoted.

P.L. Woods added ANSI-style argument
type declarations.

More cleaning up was done by
https://github.com/ncb85/SmallC-857
which was used as the basis for what
you have here.

If you contributed to Small-C before,
please get in touch with us; at least to
have you mentioned here.  Thanks!

# New In v3.2

For 'unsigned int' declarations the
'int' keyword can be omitted:

~~~C
unsigned int i;
unsigned i2;    // Same.
~~~

# Targets

Small-C is retargetable.

# Supported Targets

Currently the MOS 6502 target is under
construction.  Therefore the other
targets are disabled.

# Contributing

Contributions are highly welcomed;
please join
[Small-C v3.2 on GitHub](https://github.com/SvenMichaelKlose/).

# Language

## Comments

Comment may be K&R or ANSI-style.

~~~C
/*
  Kerningham & Ritchie style comment
*/

// ANSI one-liner
~~~

## Primitive Types

Signed or unsigned chars and ints are
supported.  Shorts are the same as
ints.

Variacle declarations cannot
have initializers with expressions in
them.

~~~C
char  c;
char  c = 0;
char  c = 0 + 1;    ; // No!
short s;
unsigned u; // Short for 'unsigned int'.
int      i;
unsigned char  uc;
unsigned short us;
unsigned int   ii;
~~~

## Pointers

Pointers are always assumed to be of
size 'int' and can be dereferenced only
once withon an expression.

~~~C
v = \*p;
v = \*\*p;  // No!
~~~

## Composite Types

### Arrays

Array contain any number of elements of
the same type and must have a known size
at declaration time.  Arrays can have
only one dimension only.

~~~C
char array1[5];
char array1[23][42]; // No.
~~~

### Structures And Unions

A 'struct' allows to group up items into
named records.  A 'union' is a struct
where all elements share the same memory
location.  'struct' and 'union' may be
mixed and nested.

Elements an be accessed via the dot
operator '.' is the type is static or
with the arrow operator '->' with a
pointer to it.  But indirections (using
the arrow operator '->') cannot be
queued up.

~~~C
struct btnode {
    char parent;
    char left;
    char right;
    struct 
};

struct btnode n;
struct btnode *parent;
parent = n.parent;
struct btnode *p = &n;
parent = n->parent;

// All members share the same memory
// location.
union {
    char same;
    char memory;
    char location;
    char for_all_four;
};
~~~

## Literal Constants

~~~
'c'         // char
"string"    // char *
23          // decimal
0x1234      // hexadecimal
0XDEADBEEF  // hexadecimal
0755        // octal
{0, 1, 2}   // Array or structure.
~~~

## Variable Initializers

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

## Functions

The must be no forward-declarations and
the return type or argument types do not
matter at call-time.  The return value
may be ommitted entirely.  It can also
be declared 'void' alongside the other
data types.

Local variable declarations have to
precede the statements.  They can be
declared static (global variable that
can be accessed from inside the function
only).

~~~C
some_fun(); // No!

// K&R-style argument types.
some_fun (to, have)
char *to;
int  have;
{
    int i;
    static int j;
    return 0;
}

// ANSI-style argument types.
some_fun (char *to, int have)
{
}
~~~

## Control Flow

'goto' is not supported (yet).

### 'return'
### 'if'/'else'
### 'break'
### 'do'/'while'
### 'for'
### 'switch'/'case'/'default'
### 'while'

## Operators

### Arithmetic Operators

#### Addition (`+`)

Adds two operands.

~~~C
a = 5;
b = 3;
c = a + b; // c is 8
~~~

#### Subtraction (`-`)

Subtracts the second operand from the
first.

~~~C
a = 5;
b = 3;
c = a - b; // c is 2
~~~

#### Multiplication (`*`)

Multiplies two operands.

~~~C
a = 5;
b = 3;
c = a * b; // c is 15
~~~

#### Division (`/`)

Divides the first operand by the second.

~~~C
a = 6;
b = 3;
c = a / b; // c is 2
~~~

#### Modulo (`%`)

Returns the remainder of division of
the first operand by the second.

~~~C
a = 5;
b = 3;
c = a % b; // c is 2
~~~

### Relational Operators

#### Equal to (`==`)

Checks if two operands are equal.

~~~C
if (a == b) { ... }
~~~

#### Not equal to (`!=`)

Checks if two operands are not equal.

~~~C
if (a != b) { ... }
~~~

#### Greater than (`>`)

Checks if the first operand is greater
than the second.

~~~C
if (a > b) { ... }
~~~

#### Less than (`<`)

Checks if the first operand is less
than the second.

~~~C
if (a < b) { ... }
~~~

#### Greater than or equal to (`>=`)

Checks if the first operand is greater
than or equal to the second.

~~~C
if (a >= b) { ... }
~~~

#### Less than or equal to (`<=`)

Checks if the first operand is less
than or equal to the second.

~~~C
if (a <= b) { ... }
~~~

### Logical Operators

#### Logical AND (`&&`)

Returns true if both statements are
true.

~~~C
if (a > 10 && b < 5) { ... }
~~~

#### Logical OR (`||`)

Returns true if at least one statement
is true.

~~~C
if (a > 10 || b < 5) { ... }
~~~

#### Logical NOT (`!`)

Reverses the logical state of its
operand.

~~~C
if (!a) { ... }
~~~

### Bitwise Operators

#### AND (`&`)

Applies the AND operation on each pair of bits.

~~~C
c = a & b; // bitwise AND
~~~

#### Inclusive OR (`|`)

Applies the OR operation on each pair of
bits.

~~~C
c = a | b; // bitwise OR
~~~

#### Exclusive OR (`^`)

Applies the XOR operation on each pair
of bits.

~~~C
c = a ^ b; // bitwise XOR
~~~

#### Complement (`~`)

Flips all the bits.

~~~C
c = ~a; // bitwise NOT
~~~

#### Left Shift (`<<`)

Shifts bits to the left, fills 0 on the
voids left as a result.

~~~C
c = a << 2; // left shift by 2
~~~

#### Right Shift (`>>`)

Shifts bits to the right, fills 0 on the
voids left as a result.

~~~C
c = a >> 2; // right shift by 2
~~~

### Assignment Operators

#### Assignment (`=`)

Assigns values from right side operands
to left side operand.

~~~C
int a = 5;
~~~

#### Add AND (`+=`)

It adds the right operand to the left
operand and assign the result to the
left operand.

~~~C
a += b; // equivalent to a = a + b
~~~

#### Subtract AND (`-=`)

It subtracts the right operand from the
left operand and assigns the result to
the left operand.

~~~C
a -= b; // equivalent to a = a - b
~~~

#### Multiply AND (`*=`)

It multiplies the right operand with the
left operand and assigns the result to
the left operand.

~~~C
a *= b; // equivalent to a = a * b
~~~

#### Divide (`/=`)

It divides the left operand with the
right operand and assigns the result to
the left operand.

~~~C
a /= b; // equivalent to a = a / b
~~~

#### Modulo (`%=`)

Takes the modulus using the two operands
and assigns the result to the left
operand.

~~~C
a %= b; // equivalent to a = a % b
~~~

### Special Assignment Operators

#### Bitwise AND assignment (&=)

Applies bitwise AND operation and
assigns the result to the left operand.

~~~C
c &= 2; // equivalent to c = c & 2
~~~

#### Bitwise OR assignment (|=)

Applies bitwise OR operation and assigns
the result to the left operand.

~~~C
c |= 2; // equivalent to c = c | 2
~~~

#### Bitwise XOR assignment (^=)

Applies bitwise XOR operation and
assigns the result to the left operand.

~~~C
c ^= 2; // equivalent to c = c ^ 2
~~~

#### Left Shift assignment (<<=)

Shifts the left operand's bits to the
left, assigns the result to the left
operand.

~~~C
c <<= 2; // equivalent to c = c << 2
~~~

#### Right Shift assignment (>>=)

Shifts the left operand's bits to the
right, assigns the result to the left
operand.

~~~C
c >>= 2; // equivalent to c = c >> 2
~~~

### List Operator

#### Comma (,)

Allows two expressions to be evaluated
in the same statement with the result of
the last expression being the value of
the combined expression.

~~~C
// b is set to 3, and then a is set to 5
a = (b = 3, b + 2);
~~~

### Increment and Decrement Operators

#### Increment (++)

Increases an integer value by one.

~~~C
++a; // pre-increment
a++; // post-increment

#### Decrement (--)

Decreases an integer value by one.

~~~C
--a; // pre-decrement
a--; // post-decrement
~~~

### Conditional Operator

#### Conditional (?:)

Returns one of two values based on the
condition given.

~~~C
// result is 'a' if a > b, otherwise 'b'
int result = (a > b) ? a : b;
~~~

### Type operators

#### Cast

Converts the value of one data type to
another.

~~~C
int b = (int) a; // casts 'a' to int.
~~~

#### sizeof()

Returns the size of a type in bytes.

~~~C
i = sizeof (char);  // 1
i = sizeof (int);   // 2
i = sizeof (struct ure); // Depends,,,
~~~

### Pointer And Member Access Operators

#### Address of (`&`)

Returns the memory address of its
operand.

~~~C
int a = 10;
int \*ptr;
ptr = &a; // ptr holds the address of a
~~~

#### Dereference (`\*`)

Accesses the value at the address
pointed to by its operand (pointer).

~~~C
int a = 10;
int* ptr;
int b;
ptr = &a;
b = *ptr; // b is 10.
~~~

#### Arrow (->)

Accesses a member of a structure or
union through a pointer.

~~~C
struct Point {int x; int y;};
struct Point *p;
*p = &point;
int x;
x = p->x; // Access x via pointer
~~~

### Array Operators

#### Subscription (`[]`)

Accesses an element at an index in an
array.

~~~C
int arr[3] = {10, 20, 30};
int x;
x = arr[1]; // x is 20
~~~

<a href="#addin_targets"></a>

# Adding New Targets Oneself

Adding support for new targets merely
involves creating a new code generator
from an existing one to print code for
an assembler (or whatever else) of your
choice.

## Code Generator Predefinitions

Define ASNM and LDNM to the names of
the assembler and linker respectively

INTSIZE is the size of an integer in
the target machine.
BYTEOFF is the offset of an byte within
an integer on the target machine. (ie:
8080,pdp11 = 0, 6809 = 1, 360 = 3)

## Compiler Architecture

Small-C is a one-pass compiler which
does preprocessing, parsing and
compiling in one go.

Internally the intermediate
representation is a two-adress machine
using two int registers instead of
addresses.  One register is called the
'primary register' as it always holds
the first operand and the result of an
operation.  The 'secondary' register
serves holding an optional second
operand or as temporary storage for
intermediate results.

Local variables (and function arguments
alongside) are stored on the CPU stack.
All arguments are of size 'int'.

Code generators need to provide 15
well-defined functions to be complete.
