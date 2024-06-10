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

# Memory usage

Lisp objects are stored on a growing
heap.

## Heap

+
| Data type               | heap      |
|-------------------------|-----------|
| cons                    | 5         |
| number (32 bit signed)  | 5         |
| symbol (also string)    | 2-257     |
| builtin function        | 2 to 257  |

TUNIX Lisp uses an object stack and an additional raw stack
alongside the CPU stack (and the C compiler's stack on
6502-CPU targets) to keep track of function calls.  Calls of
built-in functions take much less stack.

## Stacks

| Call type     | tag stack bytes | GC stack words |
|---------------|-----------------|----------------|
| built-in      | 2               | ?              |
| user-defined  | 2               | args + 1       |

## Consing

APPLY copies all but the last argument.  FUNCALL copies all
arguments.

# Data types

Symbols have an immutable, case-sensitive name of up to 255
bytes in length and a value to which they evaluate. They
can be converted to and from lists of char numbers to
facilitate strings.
**For convenience, symbols are printed in upper case
throughout this manual.**

Functions are lists starting with an argument definition
followed by statements.  The LAMBDA keyword is not
supported.

~~~lisp
; Make a function that adds X to its argument.
(fn make-adder (x)
  $((a)
     (+ a ,x)))
~~~

## Argument type definitions (of built-ins)

**Types are written in short form in this manual and
internally as argument definitions of built-in functions
(without spaces).**

| Code    | Type                                     |
|---------|------------------------------------------|
| x       | anything                                 |
| c       | cons                                     |
| l       | list (cons or NIL)                       |
| n       | number                                   |
| s       | symbol                                   |
| a       | Memory address (positive number)         |
| b       | Byte value                               |
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

| Form                | Short form  |
|---------------------|-------------|
| (quote x)           | 'x          |
| (quasiquote x)      | $x          |
| (unquote x)         | ,x          |
| (unquote-splice x)  | ,@x         |

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
# Built-in functions

## Definitions

| Definition                    | Type          |
|-------------------------------|---------------|
| (fn 'name 'args '+body)       | function      |
| (special 'name 'args '+body)  | special form  |
| (var 'name x)                 | variable      |

### Special form

Special forms are functions that take their arguments
unevaluated, like macros.

## Top-level

| Function    | Description                        |
|-------------|------------------------------------|
| (universe)  | Return list of permanent symbols.  |
| (gc)        | Free unused objects.               |
| (quit ?x)   | Return from debugger REPL.         |
| (exit n)    | Exit interpreter with code.        |

## Evaluation and flow control

| Function        | Description                            |
|-----------------|----------------------------------------|
| (quote 'x)      | Return argument unevaluated.           |
| (apply f +x)    | Call function with list of arguments.  |
| (funcall f +x)  | Call function with explicit arguments. |
| (eval x)        | Evaluate expression.                   |
| (? cond +x)     | Evaluate expression conditionally.     |
| (and +x)        | Logical AND.  Evaluate until NIL.      |
| (or +x)         | Logical OR.  Evaluate until not NIL.   |
| (block 's +x)   | Named block with expression list.      |
| (return x ?'s)  | Return from named block with value.    |
| (go 's)         | Jump to tag in named block.            |

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
  (return 'b foo)
  'c) -> b
~~~

Blocks of name NIL are used for loops.  For the purpose of
just butting up expressions, use T instead to make RETURNs
for name NIL drop through.

~~~lisp
(macro progn body
  ^(block t ; We don't want to catch returns.
     ,@body))
~~~

BLOCK also handles jumps initiated by GO.  A jump
destination, the "tag", must be the same symbol passed to
GO.  It must not be quoted.  It is an error if the tag
cannot be found in any of the parent blocks in the current
function.  If no expression follows the tag, NIL is
returned.

~~~lisp
; Print "1" and "3".
(block nil
  (print 1)
  (go jump-destination)
  (print 2)
  jump-destination
  (print 3))
~~~

## Equality

| Function  | Description                        |
|-----------|------------------------------------|
| (eq a b)  | Test if two objects are the same.  |

## Predicates

| Function      | Test on...         |
|---------------|--------------------|
| (not x)       | NIL                |
| (atom x)      | not a cons         |
| (cons? x)     | cons               |
| (symbol? x)   | symbol             |
| (number? x)   | number             |
| (builtin? x)  | built-in function  |
| (special? x)  | special form       |

All predicates except NOT return their argument.  NOT returns T instead.

## Symbols

| Function   | Description        |
|------------|--------------------|
| (= 's x)   | Set symbol value.  |
| (value s)  | Get symbol value.  |

A symbol has a name up to 255 bytes in length and a value
which initially is itself.

## Conses

| Function      | Description                          |
|---------------|--------------------------------------|
| (car l)       | Return first value of cons or NIL.   |
| (cdr l)       | Return second value of cons or NIL.  |
| (setcar c x)  | Set first value of cons.             |
| (setcdr c x)  | Set second value of cons.            |

A 'cons' points to two other objects, called 'car' and
'cdr' for historical reasons.  They could also be called
'first' and 'second', or 'head' and 'tail' in the context
of singly-linked lists.

## Lists

| Function      | Description                          |
|---------------|--------------------------------------|
| (length l)    | Return length of list.               |
| (@ f l)       | Run list items through function.     |
| (butlast l)   | Copy list but not its last element.  |
| (last l)      | Return last element.                 |
| (member x l)  | Return element containing X.         |

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
| (out x)          | Print char or plain symbol name.    |
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

| Function    | Description                     |
|-------------|---------------------------------|
| (peek a)    | Read byte from memory.          |
| (poke a b)  | Write to memory.                |
| (sys a)     | Calls machine code subroutine.  |

# Macros

## Implementation

| User-defined function  | Description          |
|------------------------|----------------------|
| (macroexpand x)        | Macro expander       |
| (quasiquote x)         | Quasiquote expander  |

If defined, MACROEXPAND is called by the REPL and the LOAD
function to expand expressions before evaluating them.
before EVALuating, if defined.  QUASIQUOTE is like a regular
function but called with arguments unevaluated.

# Environment

The environment contains a widely accepted set of functions
and macros known from most other implementations of the
Lisp programming languages.

| Function         | Desscription                   |
|------------------|-------------------------------------|
| (macro s a +b))  |
| (macro? x)       |
| (macroexpand x)  |

| Macro            | Desscription                        |
|------------------|-------------------------------------|
| (let n init +b)  | Form block with local variable.
| (with inits +b)  | Form block with local variables.

| Macro              | Desscription                     |
|--------------------|----------------------------------|
| (prog1 +b)         |
| (progn +b)         |
| (when x +b)        |
| (unless x +b)      |
| (while (x x) +b)   |
| (dolist (s x) +b)  |

## Lists

| Function     | Desscription                        |
|--------------|-------------------------------------|
| (list +x)    | Return argument list.               |
| (list? x)    | Test if argument is NIL or a cons.  |
| (c???r l)    | Nested CAR/CDR combinations.        |
| (carlist l)  | Get first elements of lists.        |
| (cdrlist l)  | Get rest elements of lists.         |
| (copy x)     | Copy atom or tree (of conses).      |
| (equal x x)  | Test if elements of tree are EQL.   |
| (find x l)   | Find element X in list.             |
| (assoc x l)  | Return list that start with X.      |

## Stacks

| Macro        | Desscription                        |
|--------------|-------------------------------------|
| (push x l)   | Destructively push onto stack L.    |
| (pop l)      | Destructively from stack L.         |

## Queues

| Function       | Desscription                        |
|----------------|-------------------------------------|
| (make-queue)   |
| (enqueue c x)  |

## Sets
## Associative lists
## Strings
## Structures
## Binary trees

# Ideas for the future

## User-defined error handling

| Variable     | Description                  |
|--------------|------------------------------|
| \*onerror\*  | User-defined error handler.  |

Called with error code and failing expression.  This can
be used to load functions on demand:

~~~lisp
(fn onerror (n x)
  (? (== n ,(get 'not-a-function
                 (read-file "error-codes.lisp")))
     ; Evaluate matching definition in environment file.
     (with-open-file f "env.lisp"
       (while (not (eof))
              nil
         (!= (read)
           (when (and (cons? !)
                      (or (eq (car x) 'var)
                          (eq (car x) 'fn)))
             (eval !)
             (return +retry!+)))))))

(= *onerror* 'onerror)
~~~

| Function    | Description                   |
|-------------|-------------------------------|
| (undef s)   | Remove symbol from universe.  |
| (gc x)      | GC with another root object.  |

On demand loading is more practical if one can get rid of
definitions.  UNDEF takes symbols out of the universe, so
the definition will leave with the next GC if it is unused.

GC could take an optional argument to specify another root
than the universe to discard everything that is not part of
an app.

~~~lisp
(gc 'appstart)
; Put APPSTART back in th universe.
(var appstart appstart)
~~~

Or one could save the current list of definitions and throw
everything out that appeared later:

~~~lisp
(var *old-defs* (universe))
(load "app.lisp")
(gc *old-defs*)
~~~

## Error handling

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


## Directory access

| Function       | Description                 |
|----------------|-----------------------------|
| (opendir n s)  | Open directory on channel.  |
| (readdir n)    | READ directory info.        |
| (mkdir s)      | Create directory.           |

## Bielefeld DB

| Function        | Description                      |
|-----------------|----------------------------------|
| (db-open a)     ; Open database.                   |
| (db-add s x)    ; Add expression with string key.  |
| (db-find s)     ; Find ID by key.                  |
| (db-read n)     ; READ by ID.                      |
| (db-close n)    ; Open database.                   |
| (undef s)       | Remove symbol from universe.     |

Embedded database to the rescue the day for large data sets.

## Defining built-ins

| Function          | Description                   |
|-------------------|-------------------------------|
| (mkbuiltin a)     ; Add built-in function.        |

Submit to your fantasy.

## Compressed conses

Conses which only store the CAR if the CDR is the next
object on the heap.  This can be done at allocation time but
would make the CDR of a compressed cons immutable and add
an extra check to each operation.
env.lisp:(fn eql (a b)
