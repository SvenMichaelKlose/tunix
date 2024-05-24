TUNIX Lisp
==========

This is a Lisp interpreter for home computers with
compacting mark-and-sweep garbage collector.

| Data type              | Bytes used on heap |
|------------------------|--------------------|
| cons                   | 5                  |
| number (16 bit signed) | 3                  |
| symbol                 | 2 to 257           |
| builtin function       | 2 to 257           |

Symbols have a case-sensitive name and a value.

Functions are lists starting with an argument definition
followed by statements.

~~~lisp
(fn make-adder (x)
  '((a)
     (+ a ,x)))
~~~

# Function reference

## Evaluation and flow control

### (quote x)

Return its argument unevaluated.

~~~lisp
(var x "What a day!")
x         -> "What a day!"
(quote x) -> x
~~~

Short form "'x" is supported by READ.

~~~lisp
(var x "What a day!")
x  -> "What a day!"
'x -> x
~~~

### (apply fun . args)

Calls function 'fun'.  The last argument must be a
list which is spliced into the previous arguments.

~~~lisp
(fn list . x
  x)

(apply list '(10 11))   -> (10 11)
(apply list 1 2 '(3 4)) -> (5 10)
~~~

### (eval x)
### (? cond expr [cond expr/default])

If the first argument evaluates to non-NIL, returns the
second argument.  Otherwise the process is repeated starting
with the third argument unless there is only one argument
left which is then returned evaluated.

~~~lisp
(? nil 1)     -> nil
(? nil 1 2)   -> 2
(? nil 1 2 3) -> 3
(? t 1 2)     -> 1
(? t)         -> nil
~~~

### (& x...)

Evaluates all arguments until one evalutates to NIL.
The value of the last evaluation is returned.

~~~lisp
(and 1 2 nil) -> nil
(and 1 2)     -> 2
~~~

### (| x...)

Evaluates all arguments until one evalutates to non-NIL.
The value of the last evaluation is returned.

~~~lisp
(or 1 nil) -> 1
(or nil 2) -> 2
~~~


### (block name . body), (return x block-name), (go tag)

Evaluates the list of expressions in 'body', returning the
value of the last unless a 'return' from the block has
been initiated.

~~~lisp
(block foo
  'a
  (return 'b 'foo)
  'c) -> b
~~~

'block' also handles jumps with 'go'.  The jump destinations
must be an atomic tag in the body that is 'eq' to the
argument of 'go'.

~~~lisp
(block nil
  (print 1)
  (go 'jmp)
  (print 2)
  jmp
  (print 3)) -> "1 3"
~~~

## Equality

| Form     | Description
| (eq a b) | Test if two objects are the same.

## Predicates

| Predicate   | Description                   |
|-------------|-------------------------------|
| (not x)     | Test if object is 'nil'.      |
| (atom x)    | Test if object is not a cons. |
| (cons? x)   | Test if object is a cons.     |
| (symbol? x) | Test if object is a symbol.   |
| (number? x) | Test if object is a number.   |

## Symbols

A symbol has a name up to 255 bytes in length and a value
which initially is itself.

### (setq symbol x): Set symbol value

Assigns the evaluated value of 'x' to the symbol value of
'symbol'.

### (symbol-value s): Get symbol value

Returns the value of a symbol.

~~~lisp
(var x) -> x
(= x 1) -> 1
(symbol-value 'x) -> 1
~~~

### (string number-list): Make symbol from char list.

Creates symbol from a list of characters.  A look-up to
re-use an existing one is not performed.

## Conses

| Function        | Description                         |
|-----------------|-------------------------------------|
| (car list)      | Return first value of cons or nil.  |
| (cdr list)      | Return second value of cons or nil. |
| (rplaca x cons) | Set first value of cons.            |
| (rplacd x cons) | Set second value of cons.           |

## Numbers

| Function  | Description           |
|-----------|-----------------------|
| (== n n)  | equal                 |
| (> n n)   | greater than          |
| (< n n)   | less than             |
| (>= n n)  | greater than or equal |
| (<= n n)  | less than or equal    |

### Arithmetics

| Function    | Description                             |
|-------------|-----------------------------------------|
| (+ n n...)  | Add numbers.                            |
| (- n n...)  | Subtract rest of numbers from first.    |
| (\* n n...) | Multiply numbers.                       |
| (/ n n...)  | Divide first number by rest of numbers. |
| (% n n...)  | Modulo of numbers.                      |
| (++ n)      | Increment (add 1).                      |
| (-- n)      | Decrement (take 1).                     |

### Bit manipulation

| Function      | Description    |
|---------------|----------------|
| (bit-and n n) | AND            |
| (bit-or n n)  | Inclusive OR.  |
| (bit-xor n n) | Exclusive OR.  |
| (bit-neg n)   | Flip all bits. |
| (>> n nbits)  | Shift right.   |
| (<< n nbits)  | Shift left.    |

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

Sets the input channel.  'stdin' is the default.  The
selected 'fn' is saved to symbol 'fnin'.

### (setout fn)

Sets the output channel.  'stdout' is the default.  The
selected 'fn' is saved to symbol 'fnout'.

### (in)

Reads char from selected input channel.  Returns NIL on
end of file or if an error occured.

### (out n/s/\*)

Prints numbers as chars and symbol names unquoted.
All other object types are output by 'print'.

### (terpri)

Prints line feed/carriage return.

### (close fn)

Closes a channel.

### (load s)

Reads and evaluates file 's'.  May be nested.

## Low level
### (peek addr)

Reads byte from memory at 'addr', which must be a positive
integer.

### (poke addr byte)
Writes 'byte' to memory at 'addr'.  Both must be positive
integers.

### (sys addr)

Calls machine code function at 'addr'.

## Special

### (fn name args body...)

Used to define functions.  Assigns the list starting with
'args' to the symbol value of 'name' unevaluated and expands
the universe by 'name'.

~~~lisp
(fn length (x)
  (? (cons? x)
     (+ 1 (length (cdr x)))
     0))
~~~

### (var name obj)

Used to define variables.  Evaluates 'obj', assigns the
result to the symbol value of 'name' and expands the
universe by 'name'.

## Miscellaneous

### (gc) - Free unused objects

Removes all unused objects from the heap and returns the
number of free bytes on the heap.

### (exit n) - Exit Lisp with code

Calls the equivalent C standard library function.

# Future extensions

Type array (256 objects maximum).
Swap tail of call stack with secondary storage.

Compressed cons (special type) with CDR pointing to previous
cons (which may also be compressed already, so there is a
variable number of bytes to step down).
