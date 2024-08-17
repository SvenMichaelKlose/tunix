TUNIX development blog
======================

Author: Sven Michael Klose <pixel@hugbox.org>

# 2024-08-17

Took it to release v0.0.2 with the debugger being much more
intuitive to use.

Tomorrow I'll be turning that sketch of a line editor I just
flushed out of my mind into reality for the Commodore home
computer consoles.  It won't take too long to add multiple
lines.  Et voilá: there'll be an editor.

And here comes the next idea to make space: movable and
resizeable stacks to adjust for more heap.  But speed is
also an issue - probably a pain up main street for line
editing even.

# 2024-08-16

Release v0.0.1 is out!  TUNIX Lisp is stable and fast enough
to go for it.  There's no chance to get rid of BUGS.md
before v1.0.0 any way.  And it's time to go public for
feedback.

The manual probably needs an introduction to Lisp as the
majority of developers has no experience with it.  It's
also great for beginners.  But then again the manual needs
a proper introduction in the first place.

For now the debugger needs some straightening out.

# 2024-08-14

TUNIX Lisp became more stable again but this is not going
fast enough which unfortunately isn't my fault entirely.
I'd prefer to be writing Lisp apps and to hack away on the
Lisp compiler.

An app that comes to mind this time is a spreadsheet to do
very German protocols to get rid of yet another brain-dead
speed head who's messing up the neighbourhood.  This is a
bloody epidemic heading for its peak.

For the VIC-20 with Ultimem expansion I was thinking of
using the RAM123 area for banking to occupy as little
address space as possible to leave it for the heap.  But
the fast version of the evaluator is around 5.5K in size,
so it won't fit in there.  setjmp() and longjmp() could be
used to jump from one function to another instead of calling
it, but that again is too much of a mess.  A graphics mode
should also be possible. That's 29K for heap and stack only
if I can manage to limit everything to an 8K range.  That
also is far too messy.  Gonna have some sleep and wait for
englightenment to hit.

# 2024-08-13

Added buggy support for saving and loading images.  And now
it's time to go for overlays to make as much free heap as
possible.

I've also tried to add a CP/M build but z88dk is behaving a
bit odd.  No matter how you name libraries, they are forced
to end on suffix '.lib'.  That's quite an inconvenience and,
as far as I can tell, unnecessary.  Unfortunately some
symbols are missing when compiling 'cvs2html' and I cannot
make heads and tails of the error messages when it comes to
linking the Lisp.  Looks like someone else has to complete
that task.  TUNIX Lisp is more of a common good if it can be
compiled for today's micro-controllers.

Hacked along more after a long break and prepared some
segmentation for overlays and throwing away init code.
It would be useful if images could be loaded by different
versions of the interpreter.  Symbols of built-ins would be
required to get looked up.  One could for example throw out
READ for some stand-alone app.  Being able to exchange
images across targets would be mind-boggling.  At least for
me.

# 2024-08-11

Some mean heisenbug is hiding someplace which escapes heavy
stress testing.  Strategies: code review, cleaning up names,
more tests and documentation.  And also compiler-writing, to
make sure that the debugger is fun to use.

cc65 also offers some ways to reduce code size.
Optimizations can be controlled with #pragma directives and
intialization code does not have to stay in memory as soon
as the interpreter has booted.  The table of built-ins is
also a waste of space as the procedure names are copied
into symbols and never touched again.  Also, when testing
if a pointer is NIL, only its high byte needs to be
checked.  All that should knock off at least 1K.

What I didn't mention in here yet is a weird idea on how to
implement bytecode.  An idea that struck me during a walk,
being busy with other kinds of things entirely.  I like it
very much because I'm sure that it's pretty novel and very
effective alike.  Am just too tired to eloborate on it
right now.

# 2024-08-10

Should have known already but only realized yesterday that
the zeropage area is not cleared by the program init code
that comes with cc65.  Am just glad to have found it.
Again it's a miracle that the Lisp started at all.

Now I'm wondering how to let ONERROR handler pass through
errors if they cannot be handled.  Delaying evaluations
until procedure return (which built-in "?" does already)
should be useful here to provide (hopefully error-free)
alternatives and continue a program.

# 2024-08-08

I've implemented compresses conses right away the next day.
It's done during "manual" GC, not triggered by the
allocator, so building lists won't crash because it makes
CDRs immutable.  I ended up fixing a lot of bugs which was
great.  But I didn't really check how much space it saved.
It added to the code size and both factors kind of broke
even.  Today I sketched an algorithm to copy the heap to
achieve compression of all lists.  This that bring about a
third more space. Again: we'll have to see the real thing.
Implementing it for multiple heaps will probably become
very interesting.

The tests of the Unix version are quite intense.  The CBM
versions tend to crash once in a while.  Seems to be a
single bug hiding somewhere.  It won't escape a vivid eye.
The set of tests isn't complete any way.

This weekend I'll be having some kind of a vacation, so the
debugger will get some extra love.  At the moment one can't
continue a program after a breakpoint kicked in.  In case of
a breakpoint the debugger REPL is called *before* an
expression is evaluated; when an error occured it is invoked
*after* an expression failed.  It would be helpful to be
able to replace an expression instead of stepping over it.
The REPL could just return a boolean to indicate that
'value' is already there.

Now to bed! Expecting chilling evenings and a cigar to
celebrate.

# 2024-08-06

Files can be written.  That means that apps can be written.
But there's still not enough heap, although there is enough
if overlays are used.  But file I/O takes extra long time.
I'm considering to pick up on that idea to have compressed
lists where the CDR of a cons is the next cons.  It's
unnerving to reload the environment either, so images must
be there – and those can be optimized to scan and reuse
duplicate object trees.

With compressed conses there is the question how much extra
code that'd make in the final binary because then there
is of course an extended cons type requiring extra checks.
Let's check or we'll barely know.

# 2024-08-05

'(universe)' has been replaced with '\*universe\*', making
it a lot easier do "overlays".  (That's keeping only parts
of a program in memory if that's not large enough.)

# 2024-08-04

I've spent the night on TUNIX Lisp, which is working like a
charm.  Includes the UNDEF function, so programs can be
overlaid, which is essential to bring the bytecode compiler
to small machines.  It'll be replaced by another strategy.
Function UNIVERSE will become variable \*UNIVERSE\* to work
it with standard functions.

# 2024-08-03

And there it is: one of the most stupid mistakes of a
complete neophyte: when evaluating arguments of a
user-defined function, the symbol values are updated
immediately after evaluation of an argument which one of the
next arguments may require.

What now?  Old symbol values are already being pushed to the
stack and new values must go there, too, before assigning
them to their argument's symbols.  The problem is that the
new values must be popped off the stack before the old ones,
so pushing pairs of old and new values is not sound.  Two
separate lists are required on the stack, so the new values
can be fetched and assigned to their sysmbols before the
function body is evaluated and the rest is done as before.

Fortunately the solution is simple: the stack pointer is
memorized to store the old values and the new values are
pushed as usual.

# 2024-08-02

Took care of the Lisp environment and added tests and fixes
and tests and fixes and left in some hardcore bugs for those
who can fix them before the crack pipe hit fades.  I'll go
for a Cohiba cigar instead if this thing works.  Tried to
take code from the tré compiler but back then updating the
worst Lisp code ever written was out of question due to time
constraints (huge app).  It goes the other way around. The
DO macro is working magic and tré could really use an update
to keep people from getting scared off.

On CBMs it's actually not as slow as it looks.  Terminal
output, especially the scrolling, takes off a lot of CPU.

There are still GC issues.  Some pointers aren't relocated.
I've mentioned some advanced diagnostics in commit
f451635c2047026bc5462bcb0fae9df39104505b:

"Perform GC\_STRESS in alloc() instead of eval0().

To catch as many dangling pointers as possible.  It's not a
mathematical proof if it passes.  Some can escape since
objects in question might not be moving and check\_lispptr()
could, for reasons yet unknown, give false positives.
check\_lispptr() will have to go all over the heap to make
sure that a pointer refers to the first byte of a valid
object.  That is rather time-consuming but should be doable
on modern targets (currently TARGET\_UNIX).  Also, all
objects should move to ensure that pointers, which haven't
been placed on the GC stack to get relocated, are spotted by
check\_lispptr().  Last but not least check\_lispptr()
should be called whenever a pointer is moved to or from
places.

These are things that are easy to implement but going down
easy road should uncover just enough bugs to keep one busy
for a couple of days."

Such things are misplaced in (b)logs.  Will probably be done
before noon tomorrow.

## Dream Time

Even if the debugger is ready for app-writing, there's still
the issue of an editor dearly missing on small systems,
especially on the VIC.  The VI I wrote in C has no undo.
I'll scribble a sketch of a Lisp version.  Am not having a
faint picture of it yet.

I'd like to also serve a Z80 platform with the first release
of the Lisp.  I only have a ZX81 with a 16K RAM expansion,
famous for sliding out of the slot if you least expect it.
The Z80 CPU knows 16-bit pointers.  That's a huge advantage
to the 6502; the code size of the interpreter will be a lot
smaller, but I cannot truly tell how much unless I get my
butt up to include the z88dk compiler into the makefiles.
A CP/M version is probably a good choice.  Disk I/O sucks
right there but it's OK to make another libsimpleio version
for it.

For the TUNIX kernel to be portable the CBM device scheme
probably should remain intact on other targets.

# 2024-07-31

Environment procedures have been split up into separate
files, loading their prerequisites on demand.  I intended to
copy over code from tré but the code created back then, 20
years ago in part, had other problems to deal with.  It
never got rewritten to look nice because I had to achieve
particular goals, like making money with apps.  So it looks
like there'll be code copied over from TUNIX Lisp to tré
instead.  The DO macro for example should have been used
more in tré.

Loading prerequisites on demand gets the CPU stack to
overflow and that is not detected yet.  There have to be
checks or there will be time wasted with tracking down those
overflows.  But for now there should be a round of healthy
sleep first.

# 2024-07-28

Added the workflow, cleaned up and made the thing ready for
the CBM KERNALs' screen editor.  With an 80 char buffer it's
useless for writing apps.

Same kind of useless is the heap size on the C16 and VIC-20.
There are several options to make them work better:

* Use another compiler.  There's LLVM-MOS and OSCAR64.
  gcc-6502 seems to be dead.  The Amsterdam Compiler Kit is
  too old and has a weird build system which includes Lua.
* Compile to assembly and use an own optimizer, just for
  this.
* Rewrite the thing in assembly.  Hell no!
* Use init code areas as heap after startup.  That won't be
  enough.  No testing required.
* Use banking.  Not all targets support it.  The VIC-20
  plus Ultimem expansion is the only CBM target that'd work
  this like a charm.
* Find repeating code and make new function.  Also not
  knocking it off.
* Use overlays.  Takes far too much time loading and saving
  memory blocks.
* Load on demand.  An index file would tell which files to
  load.  It's safe to assume that drives like the 1541
  won't be used, to there's no limitation to the number of
  files that can be in the directory.  Otherwise:
  BielefeldDB to the resue!

And least interesting one:

* Do not load all of the environemnt.  Doesn't solve the
  problem of wanting to run a compiler.

I just did it anyhow.

# 2024-07-28

It was just taking too long to find the bug in eval\_list()
which spoiled the GC\_STRESS test.  Was a little scared
because I didn't do it for too long.  Will add it into the
new Github workflow.

# 2024-07-26

The Lisp interpreter's debugger is looking good.  The
commands 's' (step), 'n' (next), 'c' (continue), and 'p'
(print) work as expected.  The interface also does not seem
to cause trouble.  Another prerelease could be done.

For actual debugging breakpoints and fixing failing code on
the spot need to be there.  To set, delete and list
breakpoints, more commands are in order.  These are easy to
implement.  It's a good idea to have user-defined breakpoint
handling functions, I guess.  The interpreter's code is far
too big already.

Before that continues, I'll read all over the C code to make
it look beatiful.  There're always coming good things out of
doing so.  Including a good feeling about having created
something nice.

# 2024-07-22

The Lisp's garbage collector passed the stress test for the
first time.  That's quite a milestone.  As long as it keeps
passing, I can sleep a lot better.

The UNDEF function causes trouble.  It copies all of the
universe list, making the whole heap move the first
time UNREF is used, except symbol 't'.  Some pointers still
escape relocation.  It's not urgent.

The best thing is: multiple heaps work!

# 2024-07-17

Had some time to fix a couple of things.  Very enjoyable.
Will have to hunt down the reason why the GC stress test
is failing – that's not so enjoyable when your neigbors
cannot stop partying.  Criminal waste of welfare money.

# 2024-07-06

An involuntary break again.  That break is a welcome
opportunity to set up more tests.  A basic pointer check
should test if the object pointed to makes sense, so we can
check validity all over the place.

# 2024-06-30

The fragmented heap turned out to become a pool of heaps
that get filled up one by one and get garbage collected
together.

# 2024-06-29

There have been great performance improvements thanks to the
new type bit layout!  Even on the C16 and VIC-20 the gain is
impressive but both are running out of heap.

The current road map to a practical development environment:

* Stress-tested garbage collection.
* Fragmented heap.
* Debugger working again
 * Stepping.
 * Breakpoints.
 * Watchpoints.
 * Memory dumps and editing.
* Dot-notated CAR, CDR and SLOT-VALUE (x., .x, x.y).
* Built-in ASSOC.
* User defined setters (e.g. '(= x. v)).
* SLOT-VALUE access to associative lists, used as objects.
* Full file and directory support.
* Environment snapshots.
* Processes (shared heap, own stacks)
* Client-server I/O.
* Screen editing.
* BielefeldDB-based function repository for on-demand
  loading.
* A bloody compiler and bytecode interpreter.

As for as the compiler goes I should be close to having laid
out the micro-passes.  Clearly, the optimizing middle-end
will add another bunch.  They are required to clean up the
mess the previous expansions leave behind.

Where tré[^tre] is using classic STRUCTs, TUNIX Lisp will
have associative lists accessed via SLOT-VALUE, leading to
better readability.
Note to self: tré should be doing that as well.

[^tre]: [tré Lisp transpiler repository)(https://github.com/SvenMichaelKlose/tre/)

So what about the C compiler?  We can savely assume that
once typing has found its way into the TUNIX Lisp compiler,
it can also be written in itself with a 6502-CPU back-end.
But we want to stick with the C world and maybe add other
languages later.

With on-demand function loading and forgetting more than one
app can reside on the heap.  With separate stacks there
would be processes.
I'm thinking stack objects on the heap.  And processes can
run in their own symbol package.  That's where forks come
into play because the common environment should be shared
at least.

Fragmented heaps would allow static memory allocations with
very low memory waste and thus to including native code.
Also new stack types could be introduced if fragments can be
reallocated or even resized.  For example, a relocating
pointer stack, which is not seen by the GC. seems to be a
handy thing if raw data is supposed to be moving with
garbage collection.

The LAMBDA keyword must be introduced for functions as
arguments.  They cannot be macro-expanded when quoted, but
they need quasiquoting to capture variables which a macro
could be compiling.

It would be nice if there was some way to speed up garbage
collection though.  It could be done with smaller maximum
relocation table sizes to avoid longer pauses.  A copying
GC is out of question.  There's some math to be done.

# 2024-06-26: No expression

Current hit list of wanted features:

## Processes

Images that can be saved, continued, forked and swapped
between.  Functions calls across images.

## Bytecode

Bytecode functions as planned are tight, relocateable and
can be loaded on demand.

## An editor

There is a VI clone already but doing one in TUNIX Lisp
feels like something else entirely.  A programmable editor
would be beyond of what the VIC-VI could ever achieve with
just C.  So I guess another replacement of a C app it is.
But let's check the bottlenecks first.

## An assembler/disassembler

Sooner or later someone will want to pimp hot spots of their
apps big time.  And now we'd like native data type
declarations in our Lisp code to generate native code, which
is very useful when crunshing numbers..

## Real NIL object and dedicated type bits

Have NIL object somewhere on zeropage and only check if the
high byte of pointer is 0.  Dedicated type bits reduce type
checks to a bit-wise AND.

# 2024-06-25: Debuggeritis

Added a stepping debugger and messed it up a bit while
merging it into the REPL, meaning that I'm just too tired to
continue coding for the rest of the day.

Looking at the tré compiler I'm not too happy with the state
of the code which grew since 1st April 2005 and became a
compiler in November 2008.  For what TUNIX Lisp needs - a
very, very simple bytecode compiler - it can be rewritten
from scratch and used as some inspiration to clean up tré.

The wanted bytecode format is very compact but there is no
sketch of an interpreter for it.

# 2024-06-22: Compiler writing again?

The remaining heap of the Lisp interpreter is getting too
small to be useful for micro computers.  The small set of
environment code costs about 6K.  There are several
strategies to cope:

* **Using another compiler**:  There is no industrial
  strength compiler other than cc65.  Oscar64 and gcc-6502
  bring on latest optimization technicques but aren't even
  close to cc65's flexibility, ANSI-C compatibility  or
  aren't production ready.  We don't want no surprises.
* **Compressed lists**: The CDR of a cons can be left out if
  the CDR object is immediately following on the heap.
  This comes at a slight peformane penalty when accessing
  conses but removes 40% space consumption (3B instead of
  5B).  That makes CDRs of compressed conses immutable.
* **Bytecode**: reduces a function call with two arguments
  from 15B (without argument objects) to 4B with an extra
  of 2B for each function used in general.  Many run-time
  checks may also be ommitted.

# 2024-06-20:

Arguments to built-in functions have always been evaluated.
Didn't really debug that issue but wrote tests until the
problem had to prove itself.

When debuggen, the stack dump is confusing at best.  A
backtrace should be there instead.

Also very important is single-step debugging and displaying
the current expression within the current function.

# 2024-06-19:

TUNIX Lisp should support the syntactical sugar of the tré
compiler.  And the GC should be interruptible again.  The
symbol chaining spoiled it, requiring a search for the next
symbol on the heap if interrupted.  No-one wants to wait
seconds for a GC to finish in real-time apps (whatever is
real-time to the user).

# 2024-06-18:

Worked mostly on quasiquote, macroexpand, error REPL and
writing tests.  Countless bugs have been fixed.  Also things
have been cleaned up, although not to full satisfaction.
(The REPL could also be the LOAD code.)
Heap is almost 11K in total on a +35K VIC-20.
I'm glad that there's no need for banking although that came
at the cost of performance.

READ is blazingly fast compared to before.  Named objects
(symbols and built-ins with at least one character in their
names) are lined up in a singly-linked list for look-ups
that don't have to go over all other objects as well.
I doubt that a hash table would make much of a difference
for most applications.

New built-in STACK dumps the objects on the GC stack and
UNDEF clears and removes symbols from the universe list.
ONERROR can be defined to catch errors.  That's great for
testing if things go wrong as expected or on-demand loading.

# 2024-06-17:

Quasiquotes and macro expansion is working.  There is an
error REPL and smoke tests now.  Countless bugs have been
fixed and we're running out of heap.  LOAD should use the
REPL loop.

I don't want to release because I hate buggy software like
\*\*\*\* at the moment and the tests cannot be complete if
errors cannot be caught to check if they happen as
expected.

The interpreter's size and performance is becoming nasty.
A maximum of 8K address space should hold the interpreter.
I'm too curious how far one can get with overlays and no
memory expansion.

# 2024-06-07: 1000 Miles

Did a very long trip to collect retro items this week.
I'm just tired and will be coding nonsense.  A set of tests
is in order.  And I'm not motivated.

As I've mentioned earlier code from the tré compiler could
be reused.  But it's not ready for compiling C without
type information.

# 2024-06-06: Tag Stack Halved In Size

I've "fixed" the stack use when calling user-defined
functions but that didn't save as much stack as I expected.
For MAKE-COUNT it only reduces the size of the tag stack.
Argument definitions are used to restore symbol values on
function return, together with the number of arguments.

The abbreviations for QUASIQUOTE ("$"), UNQUOTE (",") and
UNQUOTE-SPLICED (",@") have been added.

# 2024-06-06: Lisp: Heavy Lifting

The BLOCK special form is now part of the evaluator so it
won't affect the CPU stack any more.  32-bit numbers are
heavy lifting to a CPU-6502 already too busy with handling
pointers.  Luckily for us numbers aren't used so often in
symbolic languages.

Argument evaluation of user-defined functions actually
wastes stack.  That's trivial to fix (no biting one's own
butt to be expected).  So macros can come.  Rest arguments,
EVAL, APPLY and FUNCALL need some special care in order to
not use CPU stack.

Symbol look-up times look like a show-stopper.  Literally.
Latest when doing inter-process communication with READ and
PRINT or on-demand loading (via user-defined error handler).
At least known symbols should get a turbo.

# 2024-06-05: Lisp: Smaller and slower

Most built-in functions now have an argument definition
which reduced the code size by around 2K but performance
dropped to 3:40min for the BLOCK-TEST.  A MAKE-COUNT upped to
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
