TUNIX Lisp interpreter
======================

This is a Lisp interpreter for home computers with
compacting mark-and-sweep garbage collector.

# Data types and heap memory consumption

| Type             | Bytes    |
|------------------|----------|
| cons             | 5        |
| symbol           | 2 to 257 |
| number (16 bit)  | 3        |
| builtin function | 2 to 257 |

Symbols are case-sensitive, although printed in upper case
in this manual, and can have a length of 0.

Functions are lists starting with an argument definition
followed by statements.

~~~
(fn make-adder (x)
  '((a)
     (+ a ,x)))
~~~

# Built-in functions

## Quoting
### (quote x)

Short form "'x" is supported by READ.

## Evaluation and flow control
### (apply fun . args)
### (eval x)
### (? cond expr [cond expr/default])
### (& x...)
### (| x...)
### (block name x...), (return x block-name), (go tag)

## Predicates
### (not x)
### (eq x)
### (atom x)
### (cons? x)
### (symbol? x)
### (number? x)

## Symbols

Symbols have a name up to 255 bytes in length and a value.

### (setq symbol x): Set symbol value
### (symbol-value symbol x); Get symbol value

## Conses
### (car lst)
### (cdr lst)
### (rplaca x lst)
### (rplacd x lst)

## Numbers
### Comparing
#### (== n n)
#### (> n n)
#### (< n n)
#### (>= n n)
#### (<= n n)

### Arithmetics
#### (+ n n...)
#### (- n n...)
#### (\* n n...)
#### (/ n n...)
#### (% n n...)
#### (++ n)
#### (-- n)

### Bit manipulation
#### (bit-and n n)
#### (bit-or n n)
#### (bit-xor n n)
#### (bit-neg n)
#### (>> n nbits)
#### (<< n nbits)

## I/O
### (read)
### (print x)
### (princ x)

Prints character instead of number.

## Low level
### (peek addr)

Reads byte from memory at 'addr', which must be a positive
integer.

### (poke addr byte)
Writes 'byte' to memory at 'addr'.  Both must be positive
integers.

### (sys addr)

## Special
### (fn name args body...)

Used to define functions.  Assigns the list starting with
'args' to the symbol value of 'name' unevaluated and expands
the universe by 'name'.

### (var name obj)

Used to define variables.  Evaluates 'obj', assigns the
result to the symbol value of 'name' and expands the
universe by 'name'.

## Miscellaneous
### (gc) - Free unused objects
### (exit n) - Exit Lisp with code

# Future extensions

Type array (256 objects maximum).
Swap tail of call stack with secondary storage.

Compressed cons (special type) with CDR pointing to previous
cons (which may also be compressed already, so there is a
variable number of bytes to step down).
