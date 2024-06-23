---
title: "TUNIX Lisp"
author: "The Garbage-Collected Manual"
date: "2024-04-20"
lang: "en"
titlepage: true
titlepage-color: "389fff"
titlepage-text-color: "ffffff"
header-left: "TUNIX Lisp"
footer-left: "The garbage-collected manual?"
toc: true
footnodes-pretty: true
book: true
classoption: [oneside]
...

# Overview

This is a Lisp interpreter with dynamic scope and compacting
mark-and-sweep garbage collector.  It is portably written in
ANSI-C and offers simplified I/O for small platforms.

Platforms known to work apart from modern Unices are:

* Commodore home computers

| Machine           | Heap size      |
|------------------------|-----------------|
| Commodore C128
| Commodore C16
| Commodore C64
| Commodore Plus4
| Commodore VIC-20

## Differences to other dialects

| Most other dialects    | TUNIX Lisp      |
|------------------------|-----------------|
| backquote sign '`'     | dollar sign '$' |
| (RPLACA v l)           | (SETCAR l v)    |
| (RPLACD v l)           | (SETCDR l v)    |
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
(symbol) -> Anonymous symbol that won't get re-used.
~~~

There are no packages.

# Memory consumption

## Heap

All objects are stored on a growing heap, so allocations are
as fast as bumping the end-of-heap pointer plus boundary
check to trigger the garbage collector.

| Data type              | heap  |
|------------------------|-------|
| cons                   | 5     |
| number (32 bit signed) | 5     |
| symbol (also string)   | 6-261 |

## Stacks

| Call type    | tag stack bytes | GC stack words |
|--------------|-----------------|----------------|
| built-in     | 2               | ?              |
| user-defined | 2               | args + 1       |

Alongside the CPU stack a separate garbage-collected GC
stack holds function arguments. An additional raw stack
holds return tags of byte size instead of full return
addresses as well as raw pointers to argument definitions
for built-in functions during their evaluation.

## Hidden creation of list elements ("consing")

APPLY copies all arguments but the last one.

# Functions

Functions are lists starting with an argument definition,
followed by a list of expressions.  The result of the last
expression is returned.  The LAMBDA keyword is not around.
Anonymous functions need to be quoted instead:

~~~lisp
; Add 1 to each number in list.
(@ '((n) (++ n)) l)
~~~

The QUASIQUOTE (short form "$") can be used to emulate
lexical scope.

~~~lisp
; Make a function that adds X to its argument.
(fn make-adder (x)
  $((a)
     (+ a ,x)))
~~~

## Argument type descriptions (and definitions)

Built-in functions have character-based argument type
definitions.  They are also used, padded with spaces, to
notate argument types in this manual most of the time.

| Code | Type                                    |
|------|-----------------------------------------|
| x    | anything                                |
| c    | cons                                    |
| l    | list (cons or NIL)                      |
| n    | number                                  |
| s    | symbol                                  |
| a    | Memory address (positive number)        |
| b    | Byte value                              |
| +    | any number of following type (eg. "+n") |
| ?    | optional following type (eg. "?x")      |
| '    | unevaluated following type (eg. "'+x")  |

# Input/output

## READ and PRINT

Expressions can be read and written using built-in functions
READ and PRINT.  Strings and chars have dedicated formats:

| Type format examples | Description                    |
|----------------------|--------------------------------|
| (a . d)              | "Dotted pair"; a literal cons. |
| "string"             | String.  Escape is "\".        |
| \\A                  | Character value.               |

READ also supports abbreviations:

| Form               | Short form |
|--------------------|------------|
| (quote x)          | 'x         |
| (quasiquote x)     | $x         |
| (unquote x)        | ,x         |
| (unquote-splice x) | ,@x        |

## Input and output channel

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
         load-in      (open pathname)
         last-result  nil
         expr         nil)
    (while (not (or (err) (eof)))
           result
      (setin load-in)
      (= expr (read))
      (setin old-in)
      (= result (eval expr)))))
~~~

## Character-based I/O

# Error handling

In case of an error a new REPL is invoked so you can
provide an alternative object for the one the failed.

The error REPL first prints some information on the error.
It may look like this:

~~~
* (fnords)
In REPL: (fnords)
Eval: fnords
Error #5: Not a fun.
1*
~~~

You may

* return a correct alternative value using **QUIT**,
* stop the program entirely using **EXIT** with no arguments
  (which would exit the interpreter),
* Continue with the current LOAD or REPL using **NOERROR**.

During evaluation the I/O channels of the running program
are assigned.

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
(fn onerror (n x x)
  (? (== n 1) ; Not a function error.
     ; Evaluate matching definition in environment file.
     (with-infile f "env.lisp"
       (while (not (eof))
              nil
         (!= (read)
           (when (and (cons? !)
                      (or (eq (car !) 'var)
                          (eq (car !) 'fn))
                      (eq (cadr !) (car x)))
             (eval !)
             (return x)))))))
~~~

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
| CHANNEL         | 10   | Cannot use channel.            |
| FILE            | 11   | Error while OPENing a file.    |
| USER            | 12   | User called ERROR.             |

NOTE: The FILE code might be nonsense.

# Built-in functions

## Top-level

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

### (quit ?x): Return from error REPL.

Returns from the REPL with an alternative result for the one
that caused the error.

### (exit ?n): Exit program or interpreter with exit code.

When called without arguments the program is stopped and
control is returned to the top-level REPL.  When called
with a number that number is the exit code for the
interpreter which will terminate immediately.

## Definitions

| Form                         | Type         |
|------------------------------|--------------|
| (fn 'name 'args '+body)      | function     |
| (special 'name 'args '+body) | special form |
| (var 'name x)                | variable     |

On demand loading is more practical if one can get rid of
definitions.  UNDEF takes symbols out of the universe, so
the definition will leave with the next GC if it is unused.

### Special form

Special forms are functions that take their arguments
unevaluated, like macros.

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

TODO: Describe APPLY and FUNCALL elegantly.

Calls function FUN.  The last argument in ARGS must be a
list which is appended to the previous arguments.

~~~lisp
(fn list x
  x)

(apply list '(10 11))   -> (10 11)
(apply list 1 2 '(3 4)) -> (1 2 3 4)
~~~

### (funcall f +x): Call function with explicit arguments.

### (eval x): Evaluate expression

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
value of the last unless a RETURN from the block has
been initiated.  The name of the block passed to RETURN
has to match.  It is NIL, if not specified.

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

## Symbols

| Function   | Description                           |
|------------|---------------------------------------|
| (symbol l) | Make symbol with name from char list. |
| (= 's x)   | Set symbol value.                     |
| (value s)  | Get symbol value.                     |

## Conses

| Function     | Description                         |
|--------------|-------------------------------------|
| (car l)      | Return first value of cons or NIL.  |
| (cdr l)      | Return second value of cons or NIL. |
| (setcar c x) | Set first value of cons.            |
| (setcdr c x) | Set second value of cons.           |

A 'cons' points to two other objects, called 'car' and
'cdr' for historical reasons.  They could also be called
'first' and 'second', 'first' and 'rest' or 'head' and
'tail'.  However: they are just two object pointers
packed together.

## Lists

This functions are around because the interpreter needs them
internally.

| Function          | Description                         |
|-------------------|-------------------------------------|
| (length l)        | Return length of list.              |
| (@ f l)           | Run list items through function.    |
| (butlast l)       | Copy list but not its last element. |
| (last l)          | Return last cons of list.           |
| (member x l)      | Return element containing X.        |

### (@ f l): Filter list by function

Also handles dotted pairs, filtering the last atom if it is not NIL.

### (member x l): Return cons containing X.

Uses EQ as the predicate, instead of the usual EQL which
compares number values instead of number identity.

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

| Variable | Description            |
|----------|------------------------|
| last-in  | Last input char.       |
| last-out | Last output char.      |
| fnin     | Input channel number.  |
| fnout    | Output channel number. |

## Low-level system access

| Function   | Description                    |
|------------|--------------------------------|
| (peek a)   | Read byte from memory.         |
| (poke a b) | Write to memory.               |
| (sys a)    | Calls machine code subroutine. |

## Error handling

| Function        | Description                            |
|-----------------|----------------------------------------|
| (quit ?x)       | Return from debugger REPL              |
| (exit)          | Stop program and go to top-level REPL. |
| (error x)       | Issue a user error.                    |
| (onerror n x x) | User-defined error handler.            |
| (noerror)       | Break and continue with LOAD or REPL.  |
| (debug)         | Raises a SIGTRAP signal for debugging. |

# Macros

## Implementation

| User-defined function | Description         |
|-----------------------|---------------------|
| (macroexpand x)       | Macro expander      |
| (quasiquote x)        | Quasiquote expander |

If defined, MACROEXPAND is called by the REPL and the LOAD
function to expand expressions before evaluating them.
before EVALuating, if defined.  QUASIQUOTE is a user-defined
special form which takes its arguments unevaluated.

# Environment

The environment contains a widely accepted set of functions
and macros known from most other implementations of the
Lisp programming languages.

| Function        | Desscription                   |
|-----------------|-------------------------------------|
| (macro s a +b)) |
| (macro? x)      |
| (macroexpand x) |

| Macro           | Desscription                        |
|-----------------|-------------------------------------|
| (let n init +b) | Form block with local variable.
| (with inits +b) | Form block with local variables.

| Macro             | Desscription                     |
|-------------------|----------------------------------|
| (prog1 +b)        |
| (progn +b)        |
| (when x +b)       |
| (unless x +b)     |
| (while (x x) +b)  |
| (dolist (s x) +b) |

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

## Stacks

| Macro      | Desscription                     |
|------------|----------------------------------|
| (push x l) | Destructively push onto stack L. |
| (pop l)    | Destructively from stack L.      |

## Queues

| Function       | Desscription                        |
|----------------|-------------------------------------|
|!(make-queue)   |
|!(enqueue c x)  |

## Sets

## Associative lists

| Function        | Desscription                       |
|-----------------|------------------------------------|
|!(acons alist c) | Add key/value to associative list. |
| (assoc x l)     | Return list that start with X.     |

A list of lists where the first element of each list is the
key and the rest is the value.

## Structures
## Binary trees

# Ideas for the future

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

| Command | Description                          |
|---------|--------------------------------------|
| RETURN  | Step inside evaluation.              |
| SPACE   | Step over evaluation.                |
| b [fun] | Set breakpoint on function.          |
| b       | List all breakpoints by number.      |
| d n     | Delete breakpoint by number.         |
| (       | Start a Lisp expression to evaluate. |


## Directory access

| Function      | Description                |
|---------------|----------------------------|
| (opendir n s) | Open directory on channel. |
| (readdir n)   | READ directory info.       |
| (mkdir s)     | Create directory.          |

## Bielefeld DB

| Function     | Description                     |
|--------------|---------------------------------|
| (db-open a)  | Open database.                  |
| (db-add s x) | Add expression with string key. |
| (db-find s)  | Find ID by key.                 |
| (db-read n)  | READ by ID.                     |
| (db-close n) | Close database.                 |

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
env.lisp:(fn eql (a b)

# Interpreter internals

Random notes by now to keep things cleared up.

## The evaluator

The evaluator spans not only the code in file 'eval.c'.
It includes anything that affects control flow like ?, AND
and OR.

## Breaking evalutation on errors

Global 'char has\_error' contains the code of the recently
issued error and serves as a flag to the evaluator that the
current evaluation failed and the REPL should be called for
an alternative expression.  Therefore the evaluator checks
'has\_error' whenever a result from an evaluation is
expected.

## Adding new built-in functions

~~~C
~~~

## Bytecode function object format

A bytecode functions starts off like a regular symbol,
with a type, length and value slot.  The length tells the
number of bytes following the value.  The value points to a
regular argument definition , e.g. '(first . rest)'.

| Offset | Size  | Description                   |
|--------|-------|-------------------------------|
| 0      | 1     | Object TYPE_BCFUN             |
| 1      | 1     | Total size - 3                |
| 2      | 2     | Argument definition           |
| 3      | 1     | Stack size / object list size |
| 4      | 1     | Number of jump positions      |
| 4      | 0-31  | Object list                   |
| ?      | 0-31  | Jump positions                |
| ?      | 1-220 | Byte codes                    |

Constant objects are always referenced via the object list
with a maximum of 16 entries (at least one).

## Instruction format

The highest bit of the first byte determines if the code is
an assignment or a jump.  Jumps also contain an index into
a code offset table.

place place/obj [place/obj]...
jmp

%JNNttttt

# Real-time applications

Interruptible GC with lower threshold to keep space for
critical operations is a bad idea as a GC has to complete
at some point.
