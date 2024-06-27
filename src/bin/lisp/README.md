---
title: "((())) TUNIX Lisp"
author: "The Garbage-Collected Manual(?)"
lang: "en"
titlepage: true
titlepage-color: "389fff"
titlepage-text-color: "ffffff"
header-left: "((())) TUNIX Lisp – The Garbage-Collected Manual"
footer-left: ""
toc: true
footnodes-pretty: true
book: true
classoption: [oneside]
...

# Overview

This is a Lisp interpreter, portably written in ANSI-C.
It offers:

* Most effective memory usage through compacting
  mark-and-sweep garbage collection.
* Elegant and embeddable I/O interface.
* Sweet syntactial sugar.
* Quasi-quoting and macro expansion.
* Supplementary, compressed stack to take load off the
  CPU stack, supporting 6502-CPU architectures.

See ROADMAP and TODO files for details on the status of this
project.

Using the cc65 compiler suite, this distribution of TUNIX
Lisp is made for the following platforms:

* Commodore C128
* Commodore C16
* Commodore C64
* Commodore Plus4
* Commodore VIC-20

## Differences to other dialects

| Most other dialects    | TUNIX Lisp      |
|------------------------|-----------------|
| backquote sign '`'     | dollar sign '$' |
| (RPLACA c v)           | (SETCAR c v)    |
| (RPLACD c v)           | (SETCDR c v)    |
| (MAKE-SYMBOL x)        | (SYMBOL l)      |
| (SYMBOL-VALUE s)       | (VALUE s)       |
| (FILTER f l)           | (@ f l)         |
| (LAMBDA (args . body)) | (args . body)   |
| #\\A                   | \\A             |

MEMBER and FIND compare with EQ instead of EQL.

### Symbols

Symbols have a name and a value and they also serve as
strings that can be converted to and from character value
lists.

~~~lisp
(symbol '(\A \B \C)) -> "ABC"
(symbol) ; Anonymous symbol that won't get re-used.
~~~

## Memory consumption

### Heap

All objects are stored on a growing heap, so allocations are
as fast as bumping the end-of-heap pointer plus boundary
check to trigger the garbage collector.

| Data type              | heap  |
|------------------------|-------|
| cons                   | 5     |
| number (32 bit signed) | 5     |
| symbol (also string)   | 6-261 |

### Stacks

| Call type    | tag stack bytes | GC stack words |
|--------------|-----------------|----------------|
| built-in     | 2               | ?              |
| user-defined | 2               | args + 1       |

Alongside the CPU stack a separate garbage-collected GC
stack holds function arguments. An additional raw stack
holds return tags of byte size instead of full return
addresses as well as raw pointers to argument definitions of
built-in functions during their evaluation.

### Hidden creation of list elements ("consing")

APPLY copies all arguments but the last one.

# Definitions

FN and VAR assign expressions to a symbol which is then
added to the universe, a list of symbols the garbage
collector is starting clean-ups with.  The difference
between FN and VAR is that VAR evaluates its initialization
argument.

~~~lisp
; Define permanent, named function.
(fn welcome ()
  (out '"Hello World!")
  (terpri))

; Define permanent, named variable.
(var x nil)
~~~

# Functions

Functions are lists starting with an argument definition,
followed by a list of expressions.  The result of the last
expression is returned.  The LAMBDA keyword is not around.

~~~lisp
; Function with no arguments, returning symbol NIL.
(nil)

; Function with no arguments, returning number '3'.
(nil
  1
  2
  3)

; Function returning its argument.
((x)
  x)

; Functions returning their argument list.
(x
  x)
((first . rest)
  (cons first rest))
~~~

Anonymous functions can be used as the first element of an
expression without quoting:

~~~lisp
; Print number '100'.
(((x)
   (print (+ 1 x)))
 99)
~~~

Anonymous functions as arguments need to be quoted though:

~~~lisp
; Add 1 to each number in list.
(@ '((n) (++ n)) l)
~~~

The QUASIQUOTE (short form "$") can be used to emulate
lexical scope:

~~~lisp
; Make a function that adds X to its argument.
(fn make-adder (x)
  $((a)
     (+ a ,x)))
~~~

## Argument type descriptions (and definitions)

Built-in functions have character-based typed definitions.
They are also used, padded with spaces, to print argument
types in this manual most of the time (not only for
built-ins).

| Code | Type                                    |
|------|-----------------------------------------|
|  x   | anything                                |
|  c   | cons                                    |
|  l   | list (cons or NIL)                      |
|  n   | number                                  |
|  s   | symbol                                  |
|  a   | memory address (positive number)        |
|  b   | byte value                              |
|  +   | any number of following type (eg. "+n") |
|  ?   | optional following type (eg. "?x")      |
|  '   | unevaluated following type (eg. "'+x")  |

# Input/output

TUNIX Lisp provides I/O by the expression or character.

## READing and PRINTing expressions

Expressions can be read and written using built-in functions
READ and PRINT.  Strings and chars have dedicated formats:

| Type format examples | Description                    |
|----------------------|--------------------------------|
| (a . d)              | "Dotted pair"; a literal cons. |
| "string"             | String.  Escape is "\\".       |
| \\A                  | Character value.               |

Both also support abbreviations:

| Expression         | Abbreviation |
|--------------------|--------------|
| (quote x)          | 'x           |
| (quasiquote x)     | $x           |
| (unquote x)        | ,x           |
| (unquote-splice x) | ,@x          |

## Catching I/O errors and state

## Character-based I/O

## Input and output channel

An input and an output channel can be switched between open
streams separately using functions SETIN and SETOUT.
Symbols STDIN and STDOUT contain the standard I/O channel
numbers.

~~~lisp
; Switch to standard I/O channels.
(setin stdin)
(setout stdout)
~~~

The currently active channels numbers are in symbols FNIN
and FNOUT.

New channels are created by OPEN to access files:

~~~lisp
(fn user-defined-load (pathname)
  (with (old-in       fnin
         load-in      (open pathname)
         last-result  nil)
    (setin load-in)
    (while (not (eof))
           (progn
             (setin old-in)
             last-result)
      (= last-result (eval (read))))))
~~~

# Error handling

The debugger is invoked if an error occurs and no
user-defined function has been defined.  The debugger shows
a description of the error, followed by the current
top-level expression and the erroraneous expression
highlighted inside it by triple underscores ('___').

~~~
* (fnords)
Error #5: Not a fun.
In : (___ fnords ___)
1*
~~~

The debugger takes commands like the regular REPL.  Since an
error occured the debugger will not continue with the
current expression.  An alternative value has to be supplied
instead with QUIT:

~~~
* (fnords)
Error #5: Not a fun.
In : (___ fnords ___)
1* (quit 'fords)
~~~

but until then you can execute any code you wish, using the
I/O channels of the current program, not the standard I/O of
the debugger.

EXIT without arguments will terminate the program and return
to the top-level REPL.  You can also break execution and
continue with the next top-level REPL or LOAD expression by
calling NOERROR.

Unlike the REPL, the debugger also takes one-character
commands (which the regular REPL would interpret as symbols)
to continue through the code step by step.

| Command | Description                            |
|---------|----------------------------------------|
| e       | Use previous result to fix the error.  |
| c       | Continue program execution.            |
| s       | Execute subexpression of expression.   |
| n       | Execute expression and subexpressions. |

Stepping with 's' and 'n' cannot be used when the current
expression is erroraneous without having been fixed using
command 'e'.

~~~
* (fnords)
Error #5: Not a fun.
In: (or (___ fnords ___) (other))
1* (identity e)
content-of-symbol-e
1* 'fords
fords
1* e
In: (or (fnords) ___ (other) ___)
1*
~~~

## User-defined error handler ONERROR

| Function        | Description                           |
|-----------------|---------------------------------------|
| (onerror n x x) | User-defined error handler.           |
| (noerror)       | Break and continue with LOAD or REPL. |

If defined, user-defined function ONERROR is called on
errors, except internal ones.  Errors happening inside
ONERROR will cause it to be called again.  The handler
should return a correct replacement value (insted of QUIT)
or issue a NOERROR.

### Arguments to ONERROR

ONERROR is called with the error code, the current REPL
expression and the expression that caused the error.

~~~lisp
; SKETCH! UNTESTED!
; Load missing functions on demand.
(fn onerror (n repl ev)
  (? (== n 1) ; Not a function error.
     ; Evaluate matching definition in environment file.
     (with-infile f "env.lisp"
       (while (not (eof))
              nil
         (!= (read)
           (when (and (cons? !)
                      (or (eq (car !) 'var)
                          (eq (car !) 'fn))
                      (eq (cadr !) ev))
             (eval !)
             (return x)))))))
~~~

### Error codes

| ID (ERR_...)    | Code | Description                    |
|-----------------|------|--------------------------------|
| TYPE            | 1    | Unexpected object type.        |
| ARG\_MISSING    | 2    | One or more missing arguments. |
| TAG\_MISSING    | 3    | BLOCK tag couldn't be found.   |
| TOO\_MANY\_ARGS | 4    | Too many arguments.            |
| NOT\_FUNCTION   | 5    | Object is not a function.      |
| OUT\_OF\_HEAP   | 6    | Out of heap.  Cannot catch.    |
| UNKNOWN\_TYPE   | 7    | Internal error.                |
| NO\_PAREN       | 8    | ')' missing.                   |
| STALE\_PAREN    | 9    | Unexpected ')'.                |
| CHANNEL         | 10   | Channel not open.              |
| USER            | 12   | ERROR function was called.     |
| INTERNAL        | 13   | Internal interpreter error.    |

# Built-in functions

## General

| Function   | Description                            |
|------------|----------------------------------------|
| (universe) | Return list of permanent symbols.      |
| (gc)       | Free unused objects.                   |
| (exit ?n)  | Exit program or interpreter with code. |

### (universe): Return list of permanent symbols.

### (gc): Free unused objects.

Triggers the garbage collector.  It marks all objects linked
to the universe, compacts the heap and relocates all
pointers.

### (exit ?n): Exit program or interpreter with exit code.

When called without arguments the program is stopped and
control is returned to the top-level REPL.  When called with
a number that number is the exit code for the interpreter
which will terminate immediately.

## Definitions

| Form                         | Type         |
|------------------------------|--------------|
| (fn 'name 'args '+body)      | function     |
| (special 'name 'args '+body) | special form |
| (var 'name x)                | variable     |

### (fn 'name 'args '+body): Define permanent, named function.

### (special 'name 'args '+body): Make special form.

### (var 'name init): Define permanent, named
variable.

Special forms are functions that take their arguments
unevaluated, e.g. QUASIQUOTE (see 'quasiquote.lisp').

## Evaluation and flow control

| Function       | Description                            |
|----------------|----------------------------------------|
| (quote 'x)     | Return argument unevaluated.           |
| (apply f +x)   | Call function with list of arguments.  |
| (funcall f +x) | Call function with explicit arguments. |
| (eval x)       | Evaluate expression.                   |
| (? cond +x)    | Evaluate expression conditionally.     |
| (and +x)       | Logical AND.  Evaluate until NIL.      |
| (or +x)        | Logical OR.  Evaluate until not NIL.   |
| (block 's +x)  | Named block with expression list.      |
| (return x ?'s) | Return from named block with value.    |
| (go 's)        | Jump to tag in named block.            |

### (quote x)

Returns argument unevaluated.  Suppresses replacing symbols
by their values on evaluation.

~~~lisp
; Define variable X, containing symbol string "What a day!".
(var x "What a day!")
x         -> "What a day!"
(quote x) -> x
'x        -> x  ; Short form.
~~~

### (apply fun . args): Apply function.

Calls function FUN.  The last argument in ARGS must be a
list which is appended to the previous arguments.

~~~lisp
(fn list x
  x)

(apply list '(10 11))   -> (10 11)
(apply list 1 2 '(3 4)) -> (1 2 3 4)
~~~

### (funcall f +x): Call function.

### (eval x): Evaluate expression.

Evaluates expression X and it's subexpressions.

### (? x +x): Conditional evaluation

Returns the second argument if the first one evaluates to
non-NIL.  Otherwise the process is repeated starting with
the third argument, unless there is only one argument left
which is then the default.

~~~lisp
(? nil
   1)   -> nil
(? nil
   1
   2)   -> 2
(? nil
   1
   2
   3)   -> 3
(? t
   1
   2)   -> 1
(? t)   -> nil
~~~

### (and +x)

Evaluates all arguments in order unless one evalutates to
NIL.  The value of the last evaluation is returned.

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
value of the last unless a RETURN from the block has been
initiated.  The name of the block passed to RETURN has to
match.  It is NIL, if not specified.

~~~lisp
(block foo
  'a
  (return 'b foo)
  'c) -> b
~~~

Blocks of name NIL are used for loops.  For the purpose of
just butting up expressions use T instead to make RETURNs
for name NIL drop through.

~~~lisp
(macro progn body
  $(block t ; We don't want to catch returns.
     ,@body))
~~~

BLOCK also handles jumps initiated by GO.  A jump
destination, the "tag", must be the same symbol passed to GO
unquoted.  It is an error if the tag cannot be found in any
of the parent blocks in the current function.  If no
expression follows the tag, NIL is returned.

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

| Function    | Description                          |
|-------------|--------------------------------------|
| (eq a b)    | Test if objects are the same.        |
| (eql a b)   | Test if numbers are the equal or EQ. |
| (equal a b) | Test if trees are EQL.               |

### (eq a b): Test if objects are the same.

Tests if two objects are the very same.

Numbers usually are not as they are not looked-up for reuse
like symbols.  Use EQL instead.

### (eql a b): Test if numbers are the equal or EQ.

Like EQ except for numbers: their true values are compared
using function == instead.

### (equal a b): Test if trees are EQL.

Like EQL but traversing down conses, allowing to compare
lists and trees (lists of lists).

## Predicates

| Function     | Test on...        |
|--------------|-------------------|
| (not x)      | NIL               |
| (atom x)     | not a cons        |
| (cons? x)    | cons              |
| (symbol? x)  | symbol            |
| (number? x)  | number            |
| (builtin? x) | built-in function |
| (special? x) | special form      |

All predicates except NOT and SYMOL? return their argument
instead of T when true.

TODO: Impressive example where it's advantagous.

### (not x): NIL
### (atom x): not a cons
### (cons? x): cons
### (symbol? x): symbol
### (number? x): number
### (builtin? x): built-in function
### (special? x): special form

## Symbols

| Function   | Description                           |
|------------|---------------------------------------|
| (symbol l) | Make symbol with name from char list. |
| (= 's x)   | Set symbol value.                     |
| (value s)  | Get symbol value.                     |

### (symbol l): Make symbol with name from char list.
### (= 's x): Set symbol value.
### (value s): Get symbol value.

## Conses

| Function     | Description                         |
|--------------|-------------------------------------|
| (car l)      | Return first value of cons or NIL.  |
| (cdr l)      | Return second value of cons or NIL. |
| (setcar c x) | Set first value of cons.            |
| (setcdr c x) | Set second value of cons.           |

A 'cons' points to two other objects, called 'car' and 'cdr'
for historical reasons.  They could also be called 'first'
and 'second', 'first' and 'rest' or 'head' and 'tail'.
However: they are just two object pointers packed together.

### (car l): Return first value of cons or NIL.
### (cdr l): Return second value of cons or NIL.
### (setcar c x): Set first value of cons.
### (setcdr c x): Set second value of cons.

## Lists

This functions are around because the interpreter needs them
internally.

| Function          | Description                         |
|-------------------|-------------------------------------|
| (length l)        | Return length of list.              |
| (@ f l)           | Run elements through function.      |
| (butlast l)       | Copy list but not its last element. |
| (last l)          | Return last cons of list.           |
| (member x l)      | Return list starting with X.        |
| (remove x l)      | Copy list except element X.         |

### (butlast l): Copy list but not its last element.
### (last l): Return last cons of list.
### (length l): Return length of list.

### (member x l): Return cons containing X.

Uses EQ as the predicate.

### (remove x l): Copy list except element X.

Uses EQ as the predicate.

### (@ f l): Filter list by function

Also handles dotted pairs, filtering the last atom if it is not NIL.

## Numbers

### Comparing

| Function | Description           |
|----------|-----------------------|
| (== n n) | equal                 |
| (> n n)  | greater than          |
| (< n n)  | less than             |
| (>= n n) | greater than or equal |
| (<= n n) | less than or equal    |

### Arithmetics

| Function | Description                             |
|----------|-----------------------------------------|
| (+ n n)  | Add numbers.                            |
| (- n n)  | Subtract rest of numbers from first.    |
| (\* n n) | Multiply numbers.                       |
| (/ n n)  | Divide first number by rest of numbers. |
| (% n n)  | Modulo of numbers.                      |
| (++ n)   | Increment (add 1).                      |
| (-- n)   | Decrement (take 1).                     |

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

| Function        | Description                         |
|-----------------|-------------------------------------|
| (read)          | Read expression.                    |
| (print x)       | Print expression.                   |
| (open pathname) | Open file and return channel.       |
| (err)           | Return number of last error or NIL. |
| (eof)           | Tell if read reached end of file.   |
| (setin n)       | Set input channel.                  |
| (setout n)      | Set output channel.                 |
| (in)            | Read char.                          |
| (out x)         | Print char or plain symbol name.    |
| (terpri)        | Step to next line.                  |
| (fresh-line)    | Open line if not on a fresh one.    |
| (close n)       | Close a channel.                    |
| (load pathname) | Load and evaluate file.             |

| Variable | Description            |
|----------|------------------------|
| last-in  | Last input char.       |
| last-out | Last output char.      |
| fnin     | Input channel number.  |
| fnout    | Output channel number. |

### (read): Read expression.
### (print x): Print expression.
### (open pathname): Open file and return channel.
### (err): Return number of last error or NIL.
### (eof): Tell if read reached end of file.
### (setin channel): Set input channel.
### (setout channel): Set output channel.
### (in): Read char.
### (out x): Print char or plain symbol name.
### (terpri): Step to next line.
### (fresh-line): Open line if not on a fresh one.
### (close channel): Close a channel.
### (load pathname): Load and evaluate file.

## Raw machine access

| Function   | Description                    |
|------------|--------------------------------|
| (peek a)   | Read byte from memory.         |
| (poke a b) | Write to memory.               |
| (sys a)    | Calls machine code subroutine. |

### (peek a): Read byte from memory.
### (poke a b): Write to memory.
### (sys a): Calls machine code subroutine.

## Error handling

| Function        | Description                            |
|-----------------|----------------------------------------|
| (quit ?x)       | Return from debugger REPL              |
| (exit)          | Stop program and go to top-level REPL. |
| (error x)       | Issue a user error.                    |
| (onerror n x x) | User-defined error handler.            |
| (noerror)       | Break and continue with LOAD or REPL.  |
| (debug)         | Raises a SIGTRAP signal for debugging. |
| (debugger)      | Invoke debugger with next instruction. |

### (quit ?x): Return from debugger REPL.
### (exit): Stop program and go to top-level REPL.
### (error x): Issue a user error.
### (onerror n x x): User-defined error handler.
### (noerror): Break and continue with LOAD or REPL.
### (debug): Raises a SIGTRAP signal for debugging.
### (debugger): Invoke debugger with next instruction.

# Quasiquoting

| Form               |     | Description                  |
|--------------------|-----|------------------------------|
| (quote x)          | 'x  | Return X unevaluated.        |
| (quasiquote x)     | $x  | Unevaluated if not unquoted. |
| (unquote x)        | ,x  | Insert into QUASIQUOTE.      |
| (unquote-splice x) | ,@x | Splice into QUASIQUOTE.      |

## (quote x) | 'x: Return X unevaluated.
## (quasiquote x) | $x: Unevaluated if not unquoted.
## (unquote x) | ,x: Insert into QUASIQUOTE.
## (unquote-splice x) | ,@x: Splice into QUASIQUOTE.

# Macro system

| Function        | Desscription                    |
|-----------------|---------------------------------|
| (macro s a +b)) | Add macro function to *macros*. |
| (macro? x)      | Test if symbol is in *macros*.  |
| (macroexpand x) | Expand expression.              |

## (macro s a +b)) | Add macro function to *macros*. |
## (macro? x)      | Test if symbol is in *macros*.  |
## (macroexpand x) | Expand expression.              |

# Environment

The environment contains a widely accepted set of functions
and macros known from most other implementations of the Lisp
programming languages.

## Local variables

| Macro           | Desscription                        |
|-----------------|-------------------------------------|
| (let n init +b) | Form block with local variable.
| (with inits +b) | Form block with local variables.

### (let n init +b): Form block with local variable.
### (with inits +b): Form block with local variables.

## Control flow macros

| Macro                | Desscription                     |
|----------------------|----------------------------------|
| (prog1 +b)           | Return result of first.          |
| (progn +b)           | Return result of last.           |
| (when cond +b)       | Evaluate if condition is true.   |
| (unless cond +b)     | Evaluate if condition is false.  |
| (while (cond x) +b)  | Loop while condiiton is true.    |
| (dolist (i init) +b) | Loop over elements of a list.    |

### (prog1 +b): Return result of first.
### (progn +b): Return result of last.
### (when cond +b): Evaluate if condition is true.
### (unless cond +b): Evaluate if condition is false.
### (while (cond x) +b): Loop while condiiton is true.
### (dolist (i init) +b): Loop over elements of a list.

## Lists

| Function      | Desscription                        |
|---------------|-------------------------------------|
| (list +x)     | Return argument list.               |
| (list? x)     | Test if argument is NIL or a cons.  |
| (cadr l)...   | Nested CAR/CDR combinations.        |
| (carlist l)   | Get first elements of lists.        |
| (cdrlist l)   | Get rest elements of lists.         |
| (copy-list x) | Copy only top-level list (if tree). |
| (copy x)      | Copy tree.                          |
| (find x l)    | Find element X in list.             |

### (list +x): Return argument list.
### (list? x): Test if argument is NIL or a cons.
### (cadr l)...: Nested CAR/CDR combinations.
### (carlist l): Get first elements of lists.
### (cdrlist l): Get rest elements of lists.
### (copy-list x): Copy only top-level list (if tree).
### (copy x): Copy tree.
### (find x l): Find element X in list.

## Stacks

| Macro      | Desscription                     |
|------------|----------------------------------|
| (push x l) | Destructively push onto stack L. |
| (pop l)    | Destructively pop from stack L.  |

### (push x l): Destructively push onto stack L.
### (pop l): Destructively pop from stack L.

## Queues

| Function      | Desscription                 |
|---------------|------------------------------|
| (make-queue)  | Make queue.                  |
| (enqueue c x) | Add object X to queue C.     |

### (make-queue): Make queue.
### (enqueue c x): Add object X to queue C.

## Sets

| Function               | Desscription                   |
|------------------------|--------------------------------|
| (unique x)             | Make list a set.               |
| (adjoin x set)         | Add element to set.            |
| (intersect a b)        | Elements in both.              |
| (set-difference a b)   | B elements that are not in A.  |
| (union a b)            | Unique elements from both.     |
| (set-exclusive-or a b) | Elements that are not in both. |
| (subseq? a b)          | Test if A is subset of B.      |

### (unique x): Make list a set.
### (adjoin x set): Add element to set.
### (intersect a b): Elements in both.
### (set-difference a b): B elements that are not in A.
### (union a b): Unique elements from both.
### (set-exclusive-or a b): Elements that are not in both.
### (subseq? a b): Test if A is subset of B,

## Associative lists

| Function        | Desscription                       |
|-----------------|------------------------------------|
| (acons alist c) | Add key/value to associative list. |
| (assoc x l)     | Return list that start with X.     |

A list of lists where the first element of each list is the
key and the rest is the value.

### (acons alist c): Add key/value to associative list.
### (assoc x l): Return list that start with X.

# Ideas for the future

## Directory access

| Function      | Description                   |
|---------------|-------------------------------|
| (mkdir s)     | Create directory.             |
| (opendir n s) | Open directory on channel.    |
| (readdir n)   | Read directory info.          |
| (writedir n)  | Write partial directory info. |

### (mkdir s): Create directory.
### (opendir n s): Open directory on channel.
### (readdir n): READ directory info.
### (writedir n): Write partial directory info.

## Objects

Using dot notation and SLOT-VALUE on associative lists.

## Real-time applications

Interruptible GC with lower threshold to keep space for
critical operations is a bad idea as a GC has to complete at
some point.

| Function  | Description                  |
|-----------|------------------------------|
| (gc x)    | GC with another root object. |

GC could take an optional argument to specify another root
than the universe to discard everything that is not part of
an app.

~~~lisp
(gc 'appstart)
; Put APPSTART back in the universe.
(var appstart appstart)
~~~

Or one could save the current list of definitions and throw
everything out that appeared later:

~~~lisp
(var *old-defs* (universe))
(load "app.lisp")
(gc *old-defs*)
~~~

## Bielefeld DB

| Function     | Description                     |
|--------------|---------------------------------|
| (db-open a)  | Open database.                  |
| (db-add s x) | Add expression with string key. |
| (db-find s)  | Find ID by key.                 |
| (db-read n)  | READ by ID.                     |
| (db-close n) | Close database.                 |

### (db-open a): Open database.
### (db-add s x): Add expression with string key.
### (db-find s): Find ID by key.
### (db-read n): READ by ID.
### (db-close n): Close database.

Embedded database to the rescue the day for large data sets.

## Defining built-ins

| Function        | Description            |
|-----------------|------------------------|
| (mkbuiltin s a) | Add built-in function. |

Submit to your fantasy.

## Compressed conses

Conses which only store the CAR if the CDR is the next
object on the heap.  This can be done at allocation time but
would make the CDR of a compressed cons immutable and add
an extra check to each operation.

# Compiler

Interpretation is slow because:

## Why compiling?

Reasons for performance improvements:

* Arguments are placed on the stack.
* Arguments to known functions are expanded in advance.
* Calls of known bytecode and built-in functions from within
  bytecode functions require copying indexed pointers
  to the object stack instead of saving, assigning and
  restoring symbol values.

Unknown functions are still colled via FUNCALL.

The compiler is most simple, merely translating
expressions to bytecodes which describe function calls
and control flow.  A bytecode function may be up to
255 bytes in size and is stored like a string.  Larger
functions are split up.  Quoted function expressions will
not be compiled.[^compile-anon-funs]

[^compile-anon-funs]:
  Compiling in such functions would require the LAMBDA
  keyword to tell them apart from lists.

## The target machine: Bytecode format

A bytecode functions starts off like a regular symbol,
with a type, length and value slot.  The length tells the
number of bytes following the value.  The value points to a
regular argument definition , e.g. '(first . rest)'.

| Offset | Size  | Description                         |
|--------|-------|-------------------------------------|
| 0      | 1     | Object TYPE_BYTECODE                |
| 1      | 1     | Total size - 3                      |
| 2      | 2     | Argument definition                 |
| 3      | 1     | Local stack size / object list size |
| 4      | 1     | Code offset                         |
| 4+     | ?     | Data                                |
| 4+     | ?     | Raw data                            |
| 4+     | 1-220 | Code                                |

Constant objects are always referenced via the object list
with a maximum of 16 entries (at least one).

### Instruction format

The highest bit of the first byte determines if the code is
an assignment or a jump.  Jumps also contain an index into
a code offset table.

#### Jumps

~~~
1JJIIIII
~~~

* J: type
* I: Index into target array

#### Return from function

~~~
00000000
~~~

Assigments have a destination on the stack, a function
object, a number of arguments and the arguments.

#### Assignment:

~~~
0?DDDDDD FFFFFFFF
~~~

* D: Destination on stack
* F: Function (index into object array)

#### Argument:

~~~
EPIIIIII
~~~

* E: End of argument list flag
* P: 0: stack place 1: object
* I: Index into stack or object array

## Passes

With regular macros expanded five major passes compile
function expressions to bytecode:

* Compiler macro expansion
* Quote expansion
* Quasiuote expansion
* Lambda expansion
* Expression expansion
* Optimization
* Place expansion
* Code expansion

### Compiler macro expansion

Expands control flow special forms (BLOCK, GO, RETURN, ?,
AND, OR) to assembly-level jump and tag expressions.

| Metacode     | Description                           |
|--------------|---------------------------------------|
| (%= d f +x)  | Call F with X and assign result to D. |
| (%JMP n)     | Unconditional jump.                   |
| (%JMP-NIL n) | Jump if %0 is NIL.                    |
| (%JMP-T n)   | Jump if ~0 is not NIL.                |
| (%TAG m).    | Jump destination.                     |

#### Expansion of ?

~~~lisp
(? (a)
   (b)
   (c)

(= %0 (a))
(%jmp-nil 1)
(= %0 (b))
(%jmp 2)
(%tag 1)
(= %0 (c))
(%tag 2)
~~~

#### Expansion of AND and/or OR

#### Expansion of BLOCK

The BLOCK expander need to expand child blocks first so
that deeper RETURNs with a clashing name have precedence:

~~~lisp
(block nil
  ...
  (block nil
    ...
    (return nil)  ; Must return from the closer BLOCK NIL.
    ...))
~~~

By translating deeper BLOCK's RETURN statements to metacode
jumps, only unresolved RETURNs remain for parent BLOCKs.

### Quote expansion

Quotes are entries on the function's object list.

### Quasiuote expansion

QUASIQUOTEs need to be compiled into code using LIST and
APPEND instead.

### Lambda expansion

Inlines anonymous functions.

### Expression expansion

Breaks up nested function calls into a list of single
statement assignments.

This expression
~~~
(fun1 arg1 (fun2 (fun3) (fun4)) (fun5))
~~~

becomes this:

~~~
(%= %1 (fun3))
(%= %2 (fun4))
(%= %3 (fun2 %1 %2))
(%= %4 (fun5))
(%= %0 (fun1 %0 %3 %4))
~~~

### Optimization

Basic compression of what the macro expansions generated,
e.g. double instructions or chains of jumps created mainly
by compiler macros.

### Place expansion

Here the argument symbols are replaced by %STACK or %OBJ
expressions to denote places on the stack or on the
function's object list for assembly.

### Code expansion

* Collecting object list.
* Calculating jump destination offsets.
