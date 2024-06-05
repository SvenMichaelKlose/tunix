TUNIX blog
==========

# 2024-05-05: Lisp: Smaller and slower

Most built-in functions now have an argument definition
which reduced the code size by around 2K but performance
dropped to 3:50min for the BLOCK-TEST.  A MAKE-COUNT upped to
83 (from 39).  The stack size will shrink further.

# 2024-05-01: UNIX target + hand brake

I've added target 'unix' which made a bunch of protection
faults bubble up.  The special case where a NULL pointer
is symbol NIL is now dragging the Lisp interpreter's
performance down as cc65 cannot optimize right where it is
needed.  Now it's 2:40min instead of 1:45min for the
BLOCK-TEST looping 10,000 times.  It might get better with
the built-in argument expansion but hold your horses right
there: it's time for test code growing an environment, a
set of user-defined functions that make one's life a lot
more desireable.  By the way: CBM BASIC takes 3:48 but it
has no modulo operator which adds an addition and a
subtraction.

It takes enthusiastic attitude to go and write a compiler,
that's for sure.  You might get stuck and never get far.
You don't need the sensation of failure.  To then find out
instead that it could be taught to kids.

Let's prove that.

I intend to use TUNIX Lisp to write a C compiler even if it
takes hours to compile the kernel as long as the parts of it
are dead simple.  elegant solutions will ensue.  I can feel
it.  A tokenizer separating known strings and characters
surrounding them takes less than 30 lines of code.  A parser
can be as small but would take forever to finish.  Such
version could still serve as a reference implementation for
testing.  Naturally, testing comes first but I dearly miss
macros already.  Adding a new type is always a bad idea.
The last thing the interpreter needs is additional overhead.
So, instead, the associative list \*MACROS\* is installed
which holds all functions that serve as macros for the
MACROEXPAND.

That again brings me back to continuing with built-ins
that won't grow the CPU stack.

# 2024-05-27: Looping evaluator

eval() is not calling itself any more to evaluate arguments
or function bodies.  That's a third less CPU stack use.
Also '?' delegates evaluation of the last expression to the
evaluator loop.  A third "tag stack" has been introduced to
jump around inside the evaluator instead of having it call
itself and exhaust the CPU stack.

Arguments to built-ins are now evaluated inside eval,
reducing the number of native recursions by several
magnitudes.  Format and type checking is done based on
character string argument definitions.  They can still chose
to handle arguments themselves.  I know that jumps are
possible in C, as I emulated task switching in C for
'tensix' quite some time ago, so even that could run
stackless in a portable fashion.

A couple of new optimizations come to mind.  eval() pushes
argument names onto the GC stack, which is double
information as the argument definitions are there already.
Also, there shouldn't be an entry on the tag stack for each
argument.  The number of arguments and their old values are
required on it only.

Built-ins should be optional, depending on the requirements.
The environment should be passed a list of names of
definitions that should be run.

Written in 6502-CPU assembly the interpreter wouldn't be
bigger than 5K and I'm curious what the Oscar64 compiler
will make of it.

There are features to want:

* Macros.
* Stepping debugger.  Had one in the tré interpreter.
* Storing heap as image files.
* Image file compression, e.g. to deliver apps with more
  heap.
* Calling functions in images.  Inter-process communication
  would have to be done via I/O.
* TUNIX-VI text editor integrated (or vice versa).

Curious about:

* Super-small version without inlined getters, setters and
  predicates but function versions of them instead.  Very
  much what is nowadays called "subroutine-threaded".
  Also super-slow but might get it to run on 16K RAM
  machines.
* Sliceable GC to satisfy real-time demands.  This
  interpreter is faster than CBM BASIC and being able to
  play responsive games would be lovely.

And things nice to have:

* Loadable built-ins.
* Bytecode compiler and interpreter (ported from tré).
  Bytecodes allow for smaller programs as each cons
  occupies five bytes already.

What can I say?  This is highly motivating.

# 2024-05-26 04:00: Lisp REPL

The CPU stack size is spoiling everything.  With a few
improvements that already happened the feared 'block-test'
now completes in 1:40min instead of 3:00min and half CPU
stack is occupied in comparison.

Next is to make eval() and apply() (which is more like
'funcall') one and to put sizes and tags on the stack.
Some built-ins could return the expressions they wanted
to call eval() for before returning, using an anonymous
symbol.

# 2024-05-25: Lisp REPL

File writes work, output formatting is cleaner and we have
a REPL to start over.

# 2024-05-25: Build for c128, c64, pet, plus4 and vic20

Thanks to cc65 this was a no-brainer.  The pet Lisp isn't
working though.  I'm also a little bit lost with buggy
built-in file I/O functions.

# 2024-05-24: Lisp performance fears

cc65-vic performance has been improved by using lots of
zeropage locations.  That also saved around 0.8K code size
which is a whopping 13.9K.  Problems with CBM KERNAL I/O
code of mine are peeing me off.  Lots of C macros have to
become function calls.  It would be a whole other picture
on a CPU with 16-bit registers.

A decrementing loop takes three minutes, printing a count
every 100 rounds.  GCs every 1.500 rounds with a 10K heap.

~~~lisp
(var c 10000)

(fn block-test ()
  (out "Looping...")(terpri)
  (block nil
    tag
    (setq c (-- c))
    (? (== 0 (% c 100))
       (& (print c) (terpri)))
    (? (not (== c 0))
       (go 'tag))))

(block-test)
~~~~

# 2024-05-23: Lisp interpreter I/O added

A set of CBM KERNAL-inspired I/O functions have been added
but I got stuck for today.  READs and PRINTs with the
Bielefeld DB would be lovely.

20K heap look easy to get with an optimized linker config
and relocation before interpreter start.

# 2024-05-23: Lisp GC rocks!

The Lisp interpreter's GC passes stress tests, the scariest
checkpoint. :)  NIL and 0 are now equal which is an unusual
thing with Lisps.  I don't know any dialect going down that
road so it might be dangerous.  It's less code and a bit
faster.  Strings are required and they are already there in
the shape of symbols.  Having strings without a symbol value
would add overhead and that would eat away heap size instead
of saving some.  The CBM KERNAL way of doing I/O looks like
a good match for this.  With a REPL no-one should be
offended.  It's play time with funny demo/testing code. <:)

# 2024-05-17

Going on with the Lisp interpretr is more and more looking
like a good idea.  There's no reason for the interpreter to
cons (create lists cells that need cleaning up), a
compacting garbage collector can take multiple turns until
completion, and byte numbers can be stored in pointers,
preventing GC runs to occur overly oftern.  Also, the tail
of the call stack can be swapped out and there's still READ
and PRINT (and the Bielefeld DB) to shoot records to outer
space.  The one-block heap can be saved/loaded as an image
and new built-in functions added easily.  Now we're talking.

# 2024-05-14

Am adding support for multiple host and target compilers.
This will be interesting.

# 2024-05-14 20:11

Small-C has been merged in, but you won't recognize it any
more soon: with the plan to use a Lisp interpreter to
generate C compiler code, it'll be a whole new compiler.
Am not feeling very good about writing an interpreter or
compiler again unless there's plan with medium resolution
and snappy time spans.

# 2024-05-08 16:48

exomizer-3.0.2 and oscar64 have been added.  Will try to do
an oscar64-compiled kernel and exomizer is used all over the
place anyhow.

# 2024-05-08 01:18

A lot of work on the kernel hasn't been blogged about.
More happened to the "Bielefeld DB" which will be the
system's embedded key/value database with b-tree index and
secondary storage to free main memory.

There has to be a full-scree console or working with TUNIX
and text editing will be a pain up main street.  At least
the width of a terminal has to be 60 chars.

# 2024-04-30 19:41

Cleaned up the syscall request code a bit.  The CSV format
seems to be just right for BASIC programs but binary formats
would also be appreciated.

I'm little brain-dead because of my speed-head neighbours
but I've found some awesome ear plugs in the shop.  This
will be interesting to watch.

There's still a weird memory bank bug.  To be handled
tomorrow... 8)

# 2024-04-26 23:16

Added ownership checks of extended memory banks but nothing
new so far.

# 2024-04-24 02:34

Can mass fork, but only up to 12 processes.  There're isues
with memory allocation.  Am happy enough with how it got at
this time of day.

Good night!

# 2024-04-23 16:08

Although one of the main things on my agenda with this
project is to improve my bad debugging skills.

Some procdata seems to get destroyed.  Either by a bug in
the list/deque macros or by whatever else that has gone
rogue.  The basic deque tests could use some more checks to
really cover all details.

To detect off-scope memory writes I was just thinking of a
checksummer with sitchable configuratios that contain the
memory area location and sizem and the valid checksum for
that area.  When leaving a particular code section the
checksum can be created and checked for validity when the
section, (the only one responsible for modifying that area)
is entered again.  When developing on a TUNIX this is
invaluable.

# 2024-04-21 16:59

It just appeared to me that having a separate kernal stack
might be too much of a big deal to not implements, although
I'm not sure why yet.  The kernel is currently more of a
regular library with a strange interface to applications but
banking.  I really don't see no problems at the moment.

Looks like some basic data structures are going rogue.  Did
a two day break to start (s)porting the Small-C compiler to
Commodore 8-bitters.  That's extreme fun.  Also found a book
about systems programming which I didn't even know I own.
Bliss...  Oh, shit!  I'm a nerd.
