---
title: "TUNIX Lisp"
author: "The Garbage-Collected Manual"
date: "2024-04-20"
lang: "en"
titlepage: true
titlepage-color: "389fff"
titlepage-text-color: "ffffff"
header-left: "TUNIX Lisp"
footer-left: "The Garbage-Collected Manual"
toc: true
footnodes-pretty: true
book: true
classoption: [oneside]
...

# Overview

This is a Lisp interpreter for home computers with dynamic
scope and compacting mark-and-sweep garbage collector.

# Data types

| Data type               | Bytes used on heap  |
|-------------------------|---------------------|
| cons                    | 5                   |
| number (16 bit signed)  | 3                   |
| symbol (also string)    | 2-257               |
| builtin function        | 2 to 257            |

Symbols have an immutable, case-sensitive name of up to 255
bytes in length and a value to which they evaluate. They
can be converted to and from lists of char numbers to
facilitate strings.
For convenience, symbols are printed in upper case
throughout this manual.

Functions are lists starting with an argument definition
followed by statements.  The LAMBDA tag is not supported.

~~~lisp
; Make a function that adds X to its argument.
(fn make-adder (x)
  ^((a)
     (+ a ,x)))
~~~

Types are written in short form in this manual and
internally as argument definitions of built-in functions
(without spaces).

| Code    | Type                                     |
|---------|------------------------------------------|
| x       | anything                                 |
| c       | cons                                     |
| l       | list (cons or NIL)                       |
| n       | number                                   |
| s       | symbol                                   |
| +       | any number of following type (eg. "+n")  |
| ?       | optional following type (eg. "?x")       |
| '       | unevaluated following type (eg. "'+x")   |

# Input/output

Expressions can be read and written using built-in functions
READ and PRINT.

Strings and chars have dedicated formats:

| Type format examples   | Description                     |
|------------------------|---------------------------------|
| (a . d)                | "Dotted pair"; a literal cons.  |
| "string"               | String.  Escape is "\".         |
| \\A                    | Character value.                |

READ also supports abbreviated forms:

| Form                   | Short form                    |
|------------------------|-------------------------------|
| (quote x)              | 'x                            |
| (backquote x)          | ^x (caret for compatibility)  |
| (quasiquote x)         | ,x                            |
| (quasiquote-splice x)  | ,@x                           |

I/O is performed via a pair of channels, one for input, the
other for output.  STDIN and STDOUT contain the default
channel numbers for standard I/O.  Built-in functions SETIN
and SETOUT set them.  The currently selected channels are
stored in symbols FNIN and FNOUT.

~~~lisp
; This is done autmatically at start-up:
(setin stdin)
(setout stdout)
~~~

A new channel is created by OPEN and other functions,
depending on features compiled into the interpreter.
OPEN is generally used to open files.  Here's how to execute
a Lisp file instead of using built-in LOAD:

~~~lisp
(fn user-defined-load (pathname)
  (with (old-in       fnin
         new-in       (open pathname)
         last-result  nil
         expr         nil)
    (while (not (or (err) (eof)))
           last-result
      (setin new-in)
      (= expr (read))
      (setin old-in)
      (eval expr))
    last-result))
~~~

# Error handling

When an error occurs, a message is printed along with the
object that caused the error and you end up in the debugger
REPL shell.  You can return with a new object as an argument
to built-in function QUIT.  In top-level command mode it
understands these commands:

| Command  | Description                           |
|----------|---------------------------------------|
| RETURN   | Step inside evaluation.               |
| SPACE    | Step over evaluation.                 |
| b [fun]  | Set breakpoint on function.           |
| b        | List all breakpoints by number.       |
| d n      | Delete breakpoint by number.          |
| (        | Start a Lisp expression to evaluate.  |

# Built-in functions

## Definitions

| Form                   | Description                     |
|------------------------|---------------------------------|
| (fn name args . body)  | Define function.                |
| (var name x)           | Define variable, evaluating X.  |

## Top-level

| Function    | Description                        |
|-------------|------------------------------------|
| (universe)  | Return list of permanent symbols.  |
| (gc)        | Free unused objects.               |
| (quit ?x)   | Return from debugger REPL.         |
| (exit n)    | Exit interpreter with code.        |

## Evaluation and flow control

### (quote x)

Returns argument unevaluated.  Suppresses replacing symbols
by their values on evaluation.

~~~lisp
; Define variable X, containing symbol "What a day!".
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

Calls function FUN.  The last argument in ARGS must be a
list which appended into the previous arguments.

~~~lisp
(fn list . x
  x)

(apply list '(10 11))   -> (10 11)
(apply list 1 2 '(3 4)) -> (1 2 3 4)
~~~

Treating arguments this way supports functional programming
style elegantly. [sidekick: ADD EXAMPLE]

### (eval x)

Evaluates expression X and it's subexpressions as function
calls.  Symbols are replaced by their values.

### (? x +x)

Returns the second argument if the first one evaluates to
non-NIL.  Otherwise the process is repeated starting with
the third argument, unless there is only one argument left
which is then the default.

~~~lisp
(? nil 1)     -> nil
(? nil 1 2)   -> 2
(? nil 1 2 3) -> 3
(? t 1 2)     -> 1
(? t)         -> nil
~~~

### (and +x)

Evaluates all arguments unless one evalutates to NIL.  The
value of the last evaluation is returned.

~~~lisp
(and 1 2 nil) -> nil
(and 1 2)     -> 2
~~~

### (or +x)

Evaluates all arguments unless one evalutates to non-NIL.
The value of the last evaluation is returned.

~~~lisp
(or 1 nil) -> 1
(or nil 2) -> 2
~~~

### (block name . body), (return x block-name), (go tag)

Evaluates the list of expressions in BODY, returning the
value of the last unless a RETURN from the block has
been initiated.  The name of the block passed to RETURN
has to match.  It is NIL, if not added to RETURN.

~~~lisp
(block foo
  'a
  (return 'b 'foo)
  'c) -> b
~~~

BLOCK also handles jumps initiated by GO.  A jump
destination, the "tag", must be the same symbol passed to
GO.  It must not be quoted.  It is an error if the tag
cannot be found in any of the current blocks within a
function.  If no expression follows the tag, NIL is
returned.

~~~lisp
; Print "1" and "3".
(block nil
  (print 1)
  (go 'jump-destination)
  (print 2)
  jump-destination
  (print 3))
~~~

## Equality

| Function  | Description                       |
|-----------|-----------------------------------|
| (eq a b)  | Test if two objects are the same. |

## Predicates

| Function     | Description                   |
|--------------|-------------------------------|
| (not x)      | Test if object is 'nil'.      |
| (atom x)     | Test if object is not a cons. |
| (cons? x)    | Test if object is a cons.     |
| (symbol? x)  | Test if object is a symbol.   |
| (number? x)  | Test if object is a number.   |

## Symbols

A symbol has a name up to 255 bytes in length and a value
which initially is itself.

| Function   | Description        |
|------------|--------------------|
| (= s x)    | Set symbol value.  |
| (value s)  | Get symbol value.  |

## Conses

Conses are value pairs.

| Function      | Description                          |
|---------------|--------------------------------------|
| (car l)       | Return first value of cons or nil.   |
| (cdr l)       | Return second value of cons or nil.  |
| (setcar c x)  | Set first value of cons.             |
| (setcdr c x)  | Set second value of cons.            |

## Numbers

### Comparing

| Function  | Description           |
|-----------|-----------------------|
| (== n n)  | equal                 |
| (> n n)   | greater than          |
| (< n n)   | less than             |
| (>= n n)  | greater than or equal |
| (<= n n)  | less than or equal    |

### Arithmetics

| Function  | Description                             |
|-----------|-----------------------------------------|
| (+ n n)   | Add numbers.                            |
| (- n n)   | Subtract rest of numbers from first.    |
| (\* n n)  | Multiply numbers.                       |
| (/ n n)   | Divide first number by rest of numbers. |
| (% n n)   | Modulo of numbers.                      |
| (++ n)    | Increment (add 1).                      |
| (-- n)    | Decrement (take 1).                     |

### Bit manipulation

| Function       | Description     |
|----------------|-----------------|
| (bit-and n n)  | AND             |
| (bit-or n n)   | Inclusive OR.   |
| (bit-xor n n)  | Exclusive OR.   |
| (bit-neg n)    | Flip all bits.  |
| (>> n nbits)   | Shift right.    |
| (<< n nbits)   | Shift left.     |

## I/O

| Function         | Description                         |
|------------------|-------------------------------------|
| (read)           | Read expression.                    |
| (print x)        | Print expression.                   |
| (open pathname)  | Open file and return channel.       |
| (err)            | Return number of last error or NIL. |
| (eof)            | Tell if read reached end of file.   |
| (setin channel)  | Set input channel.                  |
| (setout channel) | Set output channel.                 |
| (in)             | Read char.                          |
| (out n/s/\*)     | Print char or plain symbol name.    |
| (terpri)         | Step to next line.                  |
| (fresh-line)     | Open line if not on a fresh one.    |
| (close channel)  | Close a channel.                    |
| (load pathname)  | Load and evaluate file.             |

| Variable  | Description             |
|-----------|-------------------------|
| last-in   | Last input char.        |
| last-out  | Last output char.       |
| fnin      | Input channel number.   |
| fnout     | Output channel number.  |

## Low-level system access

| Function          | Description                     |
|-------------------|---------------------------------|
| (peek addr)       | Read byte from memory.          |
| (poke addr byte)  | Write to memory.                |
| (sys addr)        | Calls machine code subroutine.  |
