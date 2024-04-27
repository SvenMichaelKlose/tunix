IR Code Reference
=================

IR codes operate on a two-register
machine, the 'primary' and the
'secondary' register.  They are used as
the first/left and second/right argument
to operators respectively.

The primary register always contains the
result of the last operation and can be
swapped with the top word on the stack,
which is used to store temporary values
alongside function arguments and return
addresses.  More complex representations
can be derived from this scheme if post-
processors outside the compiler.

IR codes are one byte long and may be
followed by a word or zero-terminated
string.

Here come all 77 codes a target
must implement:

# File Information

## FILEHEAD: Start of file
## FILETAIL: End of file
## SRCLINE string: Line of source code

# Segmentation

## SEGCODE: Start of code segment

Tells, that the following output should
go to the code segment.

## SEGDATA: Start of data segment

Tells, that the following output should
go to the data segment.

## DATAB word: Output a byte

~~~
    .byte ARG
~~~

## DATAW word: Output a word

~~~
    .word ARG
~~~

## BSS: Output a BSS byte

Reserves a byte at the current location
without a value.  For uninitialized BSS
area.

~~~
    .res 1
~~~

## DEFLOCAL word: Define a local label

Takes the label index as argument.

~~~
@arg:
~~~

## DEFGLOBAL symbol: Define a global label

~~~
arg:
~~~

## IMPORT symbol: Import a label

~~~
import fnord
~~~

~~~
.import fnord
~~~

## EXPORT",    's'},

## SWAP: Swap registers

# Registers

~~~
; 6502
    ldx regal
    ldy regah
    lda regbl
    sta regal
    lda regbh
    sta regah
    stx regbl
    sty regbh
~~~

## LDACI, LDBCI symbol: Load constant

~~~
; 6502
vldaci:
    lda #<arg
    sta regal
    lda #>arg
    sta regah

vldbci:
    lda #<arg
    sta regbl
    lda #>arg
    sta regbh
~~~

## LDACIG: Load address of symbol

~~~
; 6502
vldacig:
    lda #<arg
    sta regbl
    lda #>arg
    sta regbh
~~~

## LDAMC, LDAMI, LDBMI symbol: Load memory at symbol

## STAC, STAI symbol: Store primary at address of symbol

## GETC, GETUC, GETI: Load memory at address in primary into the primary.

## PUTC, PUTI: Store primary at address in secondary

# Arithmetics

## ADDSP: Add stack pointer to primary
## ADD: Add registers
## ADC word: Add constant to primary
## SUB word: Subtract secondary from primary
## NEG: Negate primary
## MUL: Multiply primary by secondary

Remainer in secondary, which is why
there is no IR for modulo.

## DIV, UDIV

# Increments/decrements
## INCA, DECA, DECB
## INC1, INC2, DEC1, DEC2

# Type conversion
## SIGNEXT: Sign-extend char in primary
## BOOL: Turn primary into a bool value
## NOT: Turn primary into a negated bool value

# Stack operations
## PUSHA, PUSHB, POPA, POPB
## SWAPSTACK, SPHL

# Subroutines

## CALL
## CALLPTR
## RET

# Bit-shifting

## ASLA, ASL, ASR
## LSR

# Bit manipulation
## AND
## OR
## XOR
## COMPA
## COMPB

# JUMPS

## JMP word: Jump to local label
## JMPZ, JMPNZ word: Conditional jump to local label
## JMPCASE

# Tests

# EQ, NE
# LT, LTE
# ULT, ULTE
# GT, GTE
# UGT, UGTE
