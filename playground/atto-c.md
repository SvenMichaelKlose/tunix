Atto-C compiler
===============

This picture showed up before Small-C came along.  Still
interesting is the go for a C-style macro assembler.

# Roadmap

An primitive 8-bit C compiler made of separate executables
to fit into small address spaces and generate 6502 assembly
code.

It must compile itself.

# Status

Bird Of A Feather.

# Dialect

## Data types

* void
* (unsigned) char
* (unsigned) int
* string
* array

Also as pointers.  That's it.
'__zp__' forces data onto the zeropage.

~~~asm
char __zp__ some_flag;
~~~

## Array

Array indexes are always of type 'unsigned char'.  Pointer
arithmetic is required to leap further.

## Function Arguments And Return Types

Instead of passing arguments on the stack one may either
apply one of the registers or a memory location whose value
will be saved on the stack at call time (unless declared
static).

These are the keywords to force data into registers:

* __a__, __x__, __y__
* __flags__, __sp__
* __ax__, __ay__
* __xy__, __yy__
* __xa__, __ya__

~~~
char __a__
balloc (void)
{
    char i;

    if (i = free_bank) {
        free_bank = banks[i];
        bank_refs[i]++;
        ALLOC_LBANK(i);
        return i;
    }
    return 0;
}
~~~
~~~
    lda free_bank
    sta i
    beq n1
    ldx i
    lda banks,x
    sta free_bank
    ldx i
    inc bank_refs,x
~~~

## Local Variables

Local variables are also placed on
global memory locations and saved on
the stack (unless that's been turned off
using the 'static' keyword.

# Compiler Architecture

1. Tokenizer
2. Identifier database
3. Pre-processor
4. Parser
5. Scope
6. Variable/function DB
7. SSA code blocks
8. Optimizations
9. Code generation
