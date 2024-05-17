TUNIX Lisp interpreter
======================

This is an extensible interpreter with mark-and-sweep
garbage collection.

The interpreter knows these data types:

* number (16 bit)
* cons
* symbol (0-255 char name)
* string (1-256 chars)
* builtin

Functions are conses with the CAR holding the argument
definition and the CDR holding the body.

# Built-in functions

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
### (symbol-value symbol x)

## Conses
### (car lst)
### (cdr lst)
### (rplaca x lst)
### (rplacd x lst)

## Quoting
### (quote x)
### (backquote x), (quasiquote x), (quasiquote-splice x)

## Numbers
### Comparison
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

## Low level
### (peek addr)
### (poke addr byte)
### (sys addr)

# Future extensions

Numbers from 1 to 255 are pointer-encoded and do not
require garbage collection.

Swap tail of call stack to secondary storage.

Type array (256 objects maximum).
