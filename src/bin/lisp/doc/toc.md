# Overview
## Some differences to other dialects
### Symbols are strings
### Built-in = instead of SETQ
### Macro ?: A more compact version of COND
## Memory consumption
### Heap
### CPU stack, object stack, and tag stack
### Inevitable creation of list elements
# About this document
## Short code examples
# Installation

# REPL, autoloader and debugger
# Symbols
# Functions
## Rest arguments
## Optional arguments
## Argument type descriptions in this manual
# Input/output
## READing and PRINTing expressions
## Catching I/O errors and state
## Character-based I/O
## Input and output channel
## Terminal control codes
# Error handling and debugging
## The debugger REPL
## Debugger short commands
## Debugger variables
## Stepping through the code
## Breakpoints
## User-defined error handler ONERROR
### Error codes
#### ERROR\_OUT\_OF\_HEAP
# Dot notation
## Real life example
# Object system
# Built-in functions
## General
### (exit ?n): Exit program or interpreter with exit code.
## Heap
### (gc): Free unused objects.
### (free): Number of free bytes on heap.
## Definitions
### Verbosity (\*V?\*, \*ALV?\*)
### (fn 'name 'args '+body): Define permanent, named function.
### (special 'name 'args '+body): Make special form.
### (var 'name init): Define permanent, named variable.
### (source s): Return defining expression for a symbol.
## Evaluation and flow control
### (quote x)
### (apply fun . args): Apply function.
### (funcall f +x): Call function.
### (eval x): Evaluate expression.
### (? x +x): Conditional evaluation
### (and +x)
### (or +x)
### (block name . body), (return x block-name), (go tag)
## Equality
### (eq a b): Test if objects are the same.
### (eql a b): Test if numbers are the equal or EQ.
### (equal a b): Test if trees are EQL.
## Predicates
### (not x): NIL
### (atom x): not a cons
### (cons? x): cons
### (symbol? x): symbol
### (number? x): number
### (builtin? x): built-in function
### (special? x): special form
## Symbols
### (symbol l): Make symbol with name from char list.
### (= 's x): Set symbol value.
### (symbol-value s): Get symbol value.
### (symbol-name s): Get name.
### (slength s): Get name length.
### (char-at s n): Char of symbol name.
## Conses
### (car l)/(cdr l): Return first or second value of cons.
### (setcar c x)/(setcdr c x): Set first/second value of cons.
## Images
## Lists
### (butlast l): Copy list but not its last element.
### (last l): Return last cons of list.
### (length l): Return length of list or name.
### (member x l): Return cons containing X.
### (nconc +l): Destructively concatenate lists.
### (remove x l): Copy list except element X.
### (@ f l): Filter list by function
## Numbers
### Comparing
### Arithmetics
### Increment/decrement
### Bit manipulation
## I/O
### (read): Read expression.
### (print x): Print expression.
### (load pathname): Load and evaluate file.
### (require +name): Load missing definition.
### (open pathname mode): Open file and channel.
### (err): Return number of last I/O error or NIL.
### (eof): Tell if last read reached end of file.
### (setin channel): Set input channel.
### (setout channel): Set output channel.
### (in): Read char.
### (conin): Read console char (non-blocking).
### (read-line): Read line as a symbol.
### (putback): Put last read char back to input.
### (out x): Print char, string or list of both.
### (outlim n): Limit number of chars printed.
### (with-in x +l): Redirect input channel for body.
### (with-out x +l): Redirect output channel for body.
### (terpri): Step to next line.
### (fresh-line): Open line if not on a fresh one.
### (close channel): Close a channel.
### (opendir): Open directory and return channel.
### (readdir n): Read first/next directory from channel.
### (closedir n): Close a directory channel.
## Time
## Raw machine access
### (rawptr a): Read byte from memory.
### (peek a): Read byte from memory.
### (poke a b): Write to memory.
### (sys a): Calls machine code subroutine.
### Example: Print memory dump
## REPL
## Error handling
### (quit ?x): Return from debugger REPL.
### (exit): Stop program and go to top-level REPL.
### (error x): Issue a user error.
### (onerror n x x): User-defined error handler.
### (ignore): Continue with next REPL expression.
### (debug): Raises a SIGTRAP signal for debugging.
### (debugger): Invoke debugger with next instruction.
# Quasiquoting
## (quote x) | 'x: Return X unevaluated.
## (quasiquote x) | $x: Unevaluated if not unquoted.
## (unquote x) | ,x: Insert into QUASIQUOTE.
## (unquote-splice x) | ,@x: Splice into QUASIQUOTE.
# Macro system
## (macro s a +x)): Define macro.
## (macro? x): Test if symbol is in \*macros\*.
## (macroexpand x): Expand expression.
# Object system
# Components
# Environment
## Local variables
### (let n init +b): Block with one local variable.
### (with inits +b): Block with many local variables.
## Control flow macros
### (prog1 +b): Return result of first.
### (progn +b): Return result of last.
### (when cond +b): Evaluate body if condition is true.
### (case x +l): Evaluate conditionally by matching value.
### (unless cond +b): Evaluate if condition is false.
## Lists
### (dup x n): Duplicate X N times
#### Example
### (list +x): Make list from arguments
### (list? x): Test if argument is NIL or a cons
### (cadr l)...: Nested CAR/CDR combinations
### (carlist l): Get first elements of lists
### (cdrlist l): Get rest elements of lists
### (append +l): Copy and append lists
### (copy-list x): Copy list
### (copy-tree x): Copy recursively
### (count-if f l): Count by predicate
### (cut-at n l): Destructively split list at position.
### (find x l): Find element X in list.
### (nth n l): Get Nth element of list.
### (nthcdr n l): Get Nth cons of list.
### (position x l ?f) | Find position of X in L.
### (split x l ?f) | Split list at object.
### (subseq l start ?end): Get sublist.
## Looping
###  DO/DO\*: Generic loop.
#### Form
#### Description
#### Examples
##### Reversed DOMTIMES
##### Iterating over a list's conses
##### Iterating over a list's conses with indexing
### (dolist (iter init) . body): Loop over list elements.
### (dotimes (iter n) . body): Loop N times.
#### Syntax
#### Example: Make list of duplicates
### (while (cond x) +b): Loop while condiiton is true
### (awhile (cond x) +b): WHILE with condition in "!"
## Stacks
### (push x l): Destructively push onto stack L.
### (pop l): Destructively pop from stack L.
## Queues
### (make-queue): Make queue.
### (enqueue c x): Add object X to queue C.
## Sets
### (unique x): Make list a set.
### (adjoin x set): Add element to set.
### (intersect a b): Elements in both.
### (set-difference a b): B elements that are not in A.
### (union a b): Unique elements from both.
### (set-exclusive-or a b): Elements that are not in both.
### (subseq? a b): Test if A is subset of B.
## Associative lists
### (acons alist c): Add key/value to associative list.
### (assoc x l): Return list that start with X.
### (aremove x l): Remove X from associative L.
### (aremove-if f l): Remove by F from associative L.
### (pairlist l l): Combine lists to associative list.
## Console control
### (clrscr): Clear screen.
### (con-xy x y): Set cursor position.
### (con-crs x): Set cursor visibility.
### (con-rvs x): Set reverse mode.
### (con-direct x): Set direct mode.
### (con-x): Get cursor X position.
### (con-y): Get cursor Y position.
## Version information
## Autoloader
## Maintainance
### (source s): Print definition of a symbol.
### (compress-tree x): Find and replace double subtrees.
#### The algorithm
## Target information
## UNIX networking sockets
### (socket-getc n): Read char from socket.
### (socket-putc n n): Put char to socket.
### (socket-accept n): Accept incoming connection.
# Compile-time options to add or remove features
## COMPRESSED\_CONS: Per-GC list compression.
## EXIT\_FAILURE\_ON\_ERROR
# Internals
## Garbage collection
## Heap object layouts
### Cones
### Numbers
### Symbols
### Built-in functions
# Adding a new target
## 1. Add a supplementary compiler configuration (cc65)
## 2. Add target to build system (GNU make)
## 3. Create build files for the compiler compiler.
## 4. Add C preprocessor definition for the target
## 5. Add default compile-time configuration.
## 6. Add libsimple I/O.
## 7. Build, fix, build, fix...
