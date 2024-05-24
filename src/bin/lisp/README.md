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

Symbols are case-sensitive and can have a length of 0.

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
### (symbol-value symbol x): Get symbol value
### (string number-list): Make symbol from char list.

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

I/O is inspired by the CBM KERNAL, which maintains a pair of
file numbers, one for input and one for output, through
which all I/O is flowing.

~~~lisp
; Open file
(? (open 4 "input.txt")
   (error "Cannot open file."))

; Select it as input.
(setin 4)

; Pass file contents through, char by char.
(while (not (eof))
  (out (in)))

; Switch back to keyboard input.
(setin stdin)

; Flush and close the file.
(close 4)
~~~

### (read)

Reads Lisp expression from selected input channel.  See
'setin'.  The default is the keyboard.

### (print x)

Prints Lisp expression to selected output channel.  See
'setout'.  The default is the screen.

### (open fn pathname)

Open file on channel 'fn'.

### (err)

Returns the status byte of the last operation.

### (eof)

Returns T if the last read reached the end of the file.

### (setin fn)

Sets the input channel. 'stdin' is the default.  The
selected 'fn' is saved to symbol 'fnin'.

### (setout fn)

Sets the output channel. 'stdout' is the default.  The
selected 'fn' is saved to symbol 'fnout'.

### (in)

Reads char from selected input channel.  'nil'/0 on end
of file.

### (out n/s/\*)

Prints numbers as chars and symbol names unquoted.

### (terpri)

Prints line feed/carriage return.

### (close fn)

Closes a channel.

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
