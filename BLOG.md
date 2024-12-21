TUNIX development blog
======================

Author: Sven Michael Klose <pixel@hugbox.org>

# 2024-12-15 Pre-processing

When building for 8-bitters, the environment files should be expanded
in advance to get around costly auto-loading.  Each file needs a
REQUIRE statement to tell the pre-processor which macros need expansion.
For compiling it should also contain required functions.
This will actually be part of the bytecode compiler.

I've added user-defined macros (UMACRO, UMACROEXPAND) of which multiple
sets may exist alongside.

~~~lisp
; Execute REQUIRE statements while pre-processing.
(umacro preproc require x
  (apply require x))

(fn preprocess (x)
  (macroexpand (umacroexpand preproc (pkgexpand (dotexpand x)))))

(fn preprocess-env (dst-dir src-dir files)
  (= dst-dir (symbol-name dst-dir))
  (= src-dir (symbol-name src-dir))
  (dolist (file files)
    (filter-file preprocess
                 (symbol (append dst-dir '(\/) file))
                 (symbol (append src-dir '(\/) file)))))
~~~

The files will be much smaller as they lose lots of whitespace, so
they'll be a lot smaller and need to be re-built if the originals
have beed modified.  Additionally:

- Replace variable names by a fixed set of anonymous symbols for
  each top-level expression.  Comes at the cost of argument definitions
  as documentation.  But:
- Build documentation database of some sort, containing the argument
  definitions and documentation strings.  Each definer could just append
  entries to a file.
- Simple optimizations to support the interpreter.

It's also a step towards replacing GNU Make.

# 2024-12-15

Spent a day on [tré](https://github.com/SvenMichaelKlose/tre/) as it
was plain broken.  Luckily, someone just sent the right ticket.
It was bad.  No-one could boot it, because I had a "local" config.
Surfed the wave and brought objects of both worlds, JavaScript and PHP,
a bit closer.  The TUNIX Lisp object piece could be placed well there.
Took about all Saturday – actually I started at around 7:00am and it's
now 03:00am.  But I squeezed in a couple of naps.  Am very happy with
the improvements.  tré has a half done rehaul of the JS/PHP object
wrappers and I wanted function overloading too.  Hard to believe that
in 2011 the compiler was able to compile itself to JS.  Apps had to
be written.

Ahyeah, right, I wanted to wrap up the assembler and plug the thing
into VICE.


# 2024-12-12

I __love__ what the environment is looking like at the moment.
Dot-expansion is now booting along as well.  Should regular code be
required, it can be expanded to a file in advance, although at the
cost of human readability: no indentation (unless a pretty PRINT gets
around) and no comments (unless included as Lisp expressions by READ).

~~~lisp
(fn expand-file (pin pout . f)
  (= f (or f. identity))
  (with-in i (open pin 'r)
    (with-out o (open pout 'w)
      (while (not (eof))
        (print (f (read)))))))

(expand-file "with-dots.lsp" "no-dots.lsp" dotexpand)
~~~

It's terribly late (03:20am).  What's still giving me the creeps is
the question of how to deal with anonymous functions (aka lambdas).
A keyword to type function expressions is required to have the compiler
compile them too.  If it can't tell if an expression is a function,
it'll have to be interpreted.  FUNCTION as that keyword is readable
better than the traditional LAMBDA.  People already unnerved by other
programming language will not watch the LAMBDA with a down-home feeling
the first time.  As far as I remember, Arc lisp is using FN.  Nice.

~~~lisp
; Old:
(@ '((x) (+ 1 x))
   numbers)

; New:
(@ (fn (x) (+ 1 x))
   numbers)
~~~

What this does not solve is closures (with lexical scope) for
both interpreter and compiler:

~~~lisp
; Old:
(@ $((x) (+ ,n x))
   numbers)

; New:
(@ (fn (x) (+ n x))
   numbers)
~~~

That new version is barely a problem to compile to but for the
interpreter it needs to get converted to the old version.  That can
be done with macro-expansion and takes away the pain of (un-)quoting
quotes.  But we want to compile functions that are already running in
the interpreter, so we need a way to tell the compiler, that an
expression is a mix of $ and FN.  We go for $FN and have that un-expand
in the compiler.  For the interpreter it's just an $.  Ooph.

~~~lisp
; Limbo lambda:
(@ ($fn (x) (+ ,n x))
   numbers)
~~~

To make it all even easier to remember, FN keywords for anonymous
functions should be optional, to make sure that traces of TUNIX Lisp
will run on an ATtiny or something alike.  One more #ifdef won't hurt.

Missed that movie again.

There's one edge case with FN for anonymous functions: a rest argument
only would be a symbol, and that tells the FN for anonymous functions
apart from global function definitions.  Deinitiely an unwanted
limitation.  We should probably stick with LAMBDA for the front end.

~~~lisp
; Revised:
(@ (lambda (x) (+ n x))
   numbers)
~~~

There is something wrong with this picture.  The code is unnecessarily
verbose with a feature that is used quite often.  The shorter FN was so
much better.  How about stealing from Common LISP?

~~~lisp
(@ #'((x) (+ n x))
   numbers)
~~~

How about getting rid of the "too many parens" topic entirely
instead?  By adding brackets:

~~~lisp
(@ [x) (+ n x)]
   numbers)

; Default argument _
(@ [+ n x]
   numbers)
~~~

That's what was intended to surface with tré Lisp.  The unmatched
closing paren tells if the symbols up to it are arguments.
Otherwise what's between the brackets is an expression if it
starts with a symbol.

~~~lisp
[) x]   ; -> (lambda () x)
[x]     ; -> (lambda (_) (x))
[x) x]  ; -> (lambda (x) (x))
~~~

And no-one has to see that LAMBDA first for a bit more code in READ.


# 2024-12-11

The autoloader is perhaps one of the most desireable features of
TUNIX Lisp.  Being able to throw out currently unused functions and
macros is priceless in constrained environments.  Following that
strategy comes at the price of having to split up the code into
smaller fragments as usual, to make wanted functions accessible via
their filenames.  The default environment already comes with +140 files.
We need something to resolve this issue without adding anything to the
interpreter.  Here's what comes to mind: use directories.
Genius, aye? m)  AUTOLOAD will have to adapt filename assembly to the
target platform (SD2IEC, BiDB access).  Let's look at some code:

~~~lisp
; Before
(fn edit file
  (= *alv?* nil)
  (clrscr)
  (reset-ln)
  (?
    file
      (progn
        (= *filename* (car file))
        (load-file))
    (and (not *lines*)
         *filename*)
      (load-file))
  (clrscr)
  (edit-lines)
  (clrscr)
  (con-direct nil)
  *filename*)

; After
(fn editor/edit file
  (= *alv?* nil)
  (clrscr)
  (reset-ln)
  (?
    file
      (progn
        (= editor/*filename* (car file))
        (editor/load-file))
    (and (not editor/*lines*)
         editor/*filename*)
      (editor/load-file))
  (clrscr)
  (editor/edit-lines)
  (clrscr)
  (con-direct nil)
  editor/*filename*)
~~~

You're not alone.  It's messing me up already as well.  The global
expansion function \*EX\*, which handles MACROEXPAND and DOTEXPAND,
could also handle symbol prefixing, based on a configuration, like:

~~~asm
; Define prefix for a set of symbols.
(in-package 'editor '(edit *lines* *filename* edit-lines))

; and to reset:
(in-package nil)
~~~

Allow IN-PACKAGE to get overriden with an empty prefix:

~~~asm
(anonymize)     ; In current IN-PACKAGE scope.
(/anonymize)    ; Top-level, default environment.
~~~

That's just another dozen lines of Lisp code to get comfy app
development.  Like the assembler.  I'm happy.  Supporting CBMs will
be its very own kind of fun.

# 2024-12-09

Am continuing work on the 6502 assembler.  An evolutionary step
compared to [Bender](https://github.com/SvenMichaelKlose/bender)
which no-one should be using.   Its use of STRUCTs alone is a punch
in the nuts.  That's also a mistake I've been pulling through
with when writing the tré Lisp compiler.  TUNIX Lisp's object system
based on associative lists has to get there too to take the noise out
of the code.

The new assembler will be a juwel added to TUNIX Lisp.  It'll
be too slow to do anything useful with on 6502 targets (and I'd like
to be proven wrong right there), but I want to compile VIC-20 projects
outside TUNIX which require lots of complicated, generated speed code.

The non-working VIC-20 version is also a pressing issue.  I cannot
post updates on Denial with no working VIC version.  With a running
version there'd even be enough heap to do something useful.  Honestly,
I've been sitting at the desk a little bit too long.

# 2024-11-16

The first compiler passes are running on unixoids and the C64.
The plus/4 and VIC-20 seem to have memory layout issues, whereas
the C128 is running out of heap.

# 2024-11-06

Was hacking something else for a change.  Am building a READ-based 6502
assembly language parser, so assembly can be integrated better.

~~~lisp
(as65 *asm-buffer*
  $((    ldx #>screen)
    (    lda #0)
    (    tay)
    (    sta p)
    (l2: stx (++ p))
    (l:  sta (p),y)
    (    iny)
    (    bne l)
    (    inx)
    (    cmp #(high (+ w screen)))
    (    bne l2)
    (    rts)))

(fn clear-screen ()
  (sys *asm-buffer*))
~~~

A built-in MALLOC and FREE it is, giving MKBUILTIN purpose. :)
Let's also see if the BiDB can be used for labels.

# 2024-10-24 - Fencing?

An object- and a filesystem.  So what?  At least I was barely coding.

# 2024-10-23 - Thoughts on a BielefeldDB-based filesystem: BiFS

The BielefeldDB (BiBD? BDB? Let's stick with BiDB) is great for
temporary databases that are discarded on program exit.  That's all due
to the memory contraints of small machines.  BiDB was supposed to fuel a
natively running version of `Small-C`, which was discared as my fingers
refused to make anything else but Lisp out of its C source.  It's worth
being carried further.

TUNIX is lacking its own filesystem for use with classic disks and
extended memory, be it RAM or Flash ROM.  It has to be robust and should
not drag down the performance of mechanical devices with increasing
fragmentation.  It must also survive system crashes, leaving the contens
of a filesystem in a valid state.

## Required BiDB features

With the idea that the BiFS nodes contain a file's name as the key,
metadata, and the payload, the lower BiDB layer must support:

* Multiple indexes in the same database to allow a database of free
  storage areas alongside, so nodes can be freed.  On overdue feature to
  add.  It'll probably take a bunch of C macros to keep the simpler
  version optional.
* Subtrees with their own indexes to implement directories.  A flag
  tells bdb\_lookup() to not traverse the children (unless being invited
  to).
* Truncation of node data.

## Required BiFS features

The filesystem has to bring on the ability to chain up blocks to a
single file and operantions for enlarging or truncating files.

The filesystem itself caches node metadata in directory files, and free
block in a free block file.  After a crash heir contents are re-built by
traversing the file tree.  To get around longer waits induced by the
intial free block scan after a crash, a small fallback area is reserved
to keep things going until the scan has completed.  Directory files can
be re-created on demand, or in the background if multi-tasking is
supported.  They just contain lists of records and are modified like any
other file.

## Disk allocation strategy

It also needs an allocation strategy, so that if two files are written
at the same time, their contents don't end up interleaved along with
considerable amounts of metadata (and time lost for extra header
updates) to chain up the fragments.  The plan: file blocks of median
size are pre-allocated to keep writes to other files out of the way and
keep the file contiguous.  They are truncated on file close.  On the
other hand it would be nice if large files wouldn't occupy a set of
neighboured tracks as that's head movements guaranteed to be added to
the overall performance.  So if a file has filled the pre-allocated
space, the next pre-allocation taking place is not following the filled
block, but at least two tracks away.  When there are no free blocks of
pre-allocation size, the largest free block is used, no matter where.

## Copy-on-update (COD) and why it's needed anyhow

Another thing that might make you wondering is copy-on-update, the idea
to never modify existing data but writing new data to a fresh location
to avoid corruption during a crash.  Including node headers this
included updating all node up to the root node for changes to become
part of the filesystem.  This assures that the current state of the
filesystem is always valid after a crash.  Some may admire this feature
being available very.  There is a good reason to implement COD: it's
required to serve Flash ROMs.  Luckily these don't require directory
files to cache node traversals, nor would a list of free block make much
sense.  The latest root node is looked up at mount time.  When a Flash
ROM is full, the filesystem needs to be defragmented with only a single
write per bank, supporting the longevity of the ROM.  Consequently
there's also a history of file versions available.  COD should allow new
nodes to share file data of an old node, which adds a bit of complexity.
COD could also be explicitly enabled or disable per branch or file, e.g.
for faster temporary files.  I seriously have no idea why everyone is
typing it COD instead of COU.

## Ultimate robustness

Finally, by tagging node headers, files can still be recovered if parent
nodes have been destroyed.

## Conclusion

The current version of the BiDB with no record deletion has around 490
lines of code (LOC).  It'll probably double up to be the filesystem as
proposed here.  But a filesystem cannot be part of TUNIX Lisp on small
machines unless the TUNIX kernel is running it as a driver in its own
address space to also hold the caches.  The BiFS could offer an
unprecedented robustness and performance advantage dearly missed and
never seen on small machines before – and do we hate those crashes, or
what?

# 2024-10-22

I've laid out a duck-typed object system with single inheritance,
based on alists.  \*PROPS\* holds the default values for each
class defined with CLASS.

~~~lisp
; (class classname . name-default-pairs)
; (No class defined before.)
(class node
  (parent    nil)
  (children  nil))

; *PROPS* now contains:
((node . ((parent   . nil)
          (children . nil))))
~~~

Methods are defined using METHOD.  It pushes argument THIS to the
front of the argument list before defining the function, and adds
it to the properties of the class it belongs to.

~~~lisp
; (member classnane methodname arguments . body)
(member node call-children (f)
  (dolist (i this.children)
    ((slot-value i f))))

; *PROPS* now has:
((node . ((call-children . ((this f)
                             (dolist (i this.children)
                               ((slot-value i f)))))
          (parent        . nil)
          (children      . nil))))
~~~

SLOT-VALUE as the function element in a call will make sure that
the evaluator adds the object to the front of the argument list
before doing the call.

~~~lisp
; (slot-value object slot-name)
((slot-value i f))

becomes

((slot-value i f) i) ; <- Object added as argument "this".
~~~

An object is created with NEW.  It copies the properties of the
wanted class and overwrites them with new values specified as pairs
in the rest of its arguments.  If a slot was not defined with CLASS,
it is added quietly.

~~~lisp
(new node extra-info "Debug here!")

; Returned object:
((extra-info    . "Debug here!")
 (call-children . ((this f)
                    (dolist (i this.children)
                      ((slot-value i f)))))
 (parent        . nil)
 (children      . nil))

; Call "obj"'s method to call each children's method RENDER.
(obj.call-children 'render)
~~~

Objects can be used as prototypes to create new classes.

~~~lisp
; (make-class classnane object)
(make-class lm-max-w (new lm w 'max))
~~~

The need for calling a method of the same name as the currently evaluated
method will be inevitable for object-oriented programming.  One solution
would be to prefix methods with their classname to be able to tell them
apart.  Given the memory constraints of small machines that's a no-go.
But since all properties including those of the parent are in the object
in the order of inheritance, it would suffice to start looking for a method
in inherited properties of the wanted class.  That's where the %TYPE slot
comes into play.  It's at the front of each inherited class' set of properties.
Now looking up a method by type would only require the use of MEMBER-IF to
find the right set of properties.

~~~lisp
; Example representation of a RECT object derived from NODE.
; The %TYPE slot allows to find the CALLBACK method of the
; NODE.
((x        . 0)
 (y        . 0)
 (w        . 0)
 (h        . 0)
 (callback . nil)
 (%type         . node)
 (callback      . nil)
 (extra-info    . "Debug here!")
 (call-children . ((this f)
                    (dolist (i this.children)
                      ((slot-value i f)))))
 (parent        . nil)
 (children      . nil))
~~~

Constructors can be defined as slot CONSTRUCTOR.  When calling
NEW, CONSTRUCTOR contains the arguments to the constructor that is
called by NEW.

That's all of it so far – the implementation amounts to a whopping
19 lines of code.

So what are we gonna do with these new powers?  How about defining
user interfaces as LML documents and having a React-like core hold
and render them to text or graphics, while maintaining state and
managing layout?  Most of the code is already in src/bin/desktop,
written in C, as an example.

# 2024-10-22

DOTEXPAND works after fixing SYMBOL, which was supposed to re-use
existing symbols but didn't.  It hasn't been enabled by default as
things are slow enough already without compiler magic.  At the moment
TUNIX Lisp can do a bit more than 330 calls to built-ins per second on
a C64.

DOTEXPAND brings an abbreviation for SLOT-VALUE to access object slots.
I'll be using associative lists to implement single inheritance and
tweak eval0() to add a hidden object argument to method calls via
SLOT-VALUE, so "(obj.fun)" becomes "(obj.fun obj)".  The question is
now how to get the most power out of a most simple object system.
Taking a look at overloading functions might spark some ideas.

# 2024-10-21

I have created a rather tight bytecode interpreter in 6502 assembly.
Each function has a stack frame and on each call it is decided if the
return value is pushed onto the stack.  Other than calls of built-in
(fastest) or a bytecode function again, super-fast internal codes get
and set stack places, jump within the function or return from it.
Objects and address pointers of built-ins are stored in a table at the
start of the function.  The GC can tell between them easily.  It is told
by the bytecode if a called function is a built-in and if it wants to
push the value in AX on the stack.  Everything else is done by built-in
functions.  If an unknown function needs to be called, an expression has
to be constructed for built-in EVAL to do the job.  All in all the
interpreter should be able to handle around 15k codes per second on a
VIC-20/C64/C128, the called functions not included.

Unfortunately that all doesn't help with the time spent by the garbage
collector.  It'll be interesting to watch in action.
Debugging will require type checks to built-ins wedged in.

# 2024-10-20

Should take a little break to get back on track.  I'm low on caffeine.

As the goal is to have something nice to code I want dot-notation in to
reduce some noise from the code picture.  Let's take a look at an
expression expander for the compiler with no dot-notation:

~~~lisp
(fn exex-move-arg (x)
  (? (cons? x)
     (!= (symbol)
       (cons ! (list (list '= ! x))))
     (cons x nil)))

(fn exexpand (x)
  (?
    (atom x)
      (list x)

    (eq '= (car x))
      (? (atom (caddr x))
         (list x)
         (!= (@ exex-move-arg (cdr (caddr x)))
           (append (mapcan exexpand (mapcan cdr !))
                   (list (list '= (cadr x) (cons (car (caddr x)) (@ car !)))))))

    (!= (@ exex-move-arg (cdr x))
      (append (mapcan exexpand (mapcan cdr !))
              (list (cons (car x) (@ car !)))))))
~~~

And now with dot-notation:

~~~lisp
(fn expex-move-arg (x)
  (? (cons? x)
     (!= (symbol)
       (. ! (list (list '= ! x))))
     (. x nil)))

(fn exexpand (x)
  (?
    (atom x)
      (list x)

    (eq '= x.)
      (? (atom ..x.)
         (list x)
         (!= (@ exex-move-arg (cdr ..x.))
           (append (mapcan exexpand (mapcan cdr !))
                   (list (list '= .x. (. (car ..x.) (@ car !)))))))

    (!= (@ exex-move-arg .x)
      (append (mapcan exexpand (mapcan cdr !)
              (list (. x. (@ car !)))))))
~~~

That's right.  COBOL-style indexing.

# 2024-10-18

Looking at the generated 6502 is giving each day a touch of unhappiness.
Also, that pile of C code, messed up with preprocessor directives and macros,
when we know that Lisp is "so powerful".  There are also essential optimizations
that cannot possibly be implemented in C (unless there's a stay in the nut house
on the todo list).  Storing a pointer index separately, for example, and word
aligning the object stack, makes pushes and pops much faster.  Only works wit
a word-aligned stack:

~~~asm

spl: .res 1
sp:  .res 1 ; (0)
sph: .res 1

; Push AX onto object stack.
pushxa:
    ldy spl         ; 3
    bne l           ; 2/3
    dec sph         ; 5
l:  dey             ; 2
    sta (sp),y      ; 6
    txa             ; 2
    dey             ; 2
    sta (sp),y      ; 6
    sty spl         ; 3
                    ; 26/32

; Pop from object stack into AX.
popxa:
    ldy spl         ; 3
    lda (sp),y      ; 6
    tax             ; 2
    iny             ; 2
    lda (sp),y      ; 6
    iny             ; 2
    bne l           ; 2/3
    inc sph         ; 5
l:  sty spl         ; 3
                    ; 26/32
~~~

Indirect access requires the low byte to be in place though:

~~~asm
; Load stack place `i` into AX.
ldspi:
    lda spl
    sta sp
    ldy #(++ i)
    lda (sp),y
    tax
    dey
    lda (sp),y
    ldy #0
    sty sp
~~~

It's still more compact than doing the addition manually and saves a word
on the zeropage.  Furthermore automatic optimization will come into play.
I've sketched a set of
[code generation macros](src/bin/lisp/growroom/igen-6502.lsp).  This time
overloaded, so optimizations and native types can be supported.

That's just dreaming about.  More useful would be having an assembler that
merges with Lisp expressions as described.  Peeking chars and READ, and a
bit of line parsing should do the trick.  But sure as hell there won't be
enough heap to load all labels of an application to make an executable.
That's where the BiDB comes in.

# 2024-10-17

While bullshiting about the overall project to kill time it occured to
me that perhaps the conses could be allocated from the opposite side
of the heap to spare the type byte of each.  Lets figure out what that
would imply.

* Type checks would require an additional pointer check to determine
  if it's a cons (with no type byte to access) or not.
* With 16 bits address space high byte checks would do.
* It only works with the first or last heap.

# 2024-10-14

The CBM part of the "simple I/O" library is doing my head in.  Test
"test-file.lsp" won't pass.  The interpreter is almost twice as fast
than it was yesterday though.  I'll use the socket support for testing
in headleass instances of VICE, so test can be run automatically, also
in "Github workflows".

# 2024-10-12

Gave the evluator a long desired clean-up.  It's a bit faster and uses
less object stack.

It's getting messed up on the Commodore plus/4 now.  The VIC-20's heap
has gotten to small, and the C16 version doesn't work anyway.
Everything's fine on Unix though.  Still need to find some issue with
libsimpleio-cbm.  It's unnerving.

That brought me to the wants of a source-level debugger for VICE.  It
provides a binary monitor via Unix networking.  Now TUNIX Lisp has
most basic networking socket support.

# 2024-10-11

I'm in desperate need for more heap to keep things convenient on CBMs.
Fun facts on the VIC:

* The maximum code window we can sacrifice is 8K.
* We'd like to just sacrifice 3K on Ultimems.
* cc65 library code is 5.5K
* simpleio is ~2.5K (should be in a kernel driver)
* Evaluator is a bit more than 4K.
* Built-ins are about 8K.
* Rest of interpreter is about 8K.

I wouldn't know how to split up eval0() into 4K chunks.

# 2024-10-08

I've added a buffer to the stacks so they won't trash the rest of the
program on overflows which are only checks once per EVAL.  I'm bad at
debugging this shit.

# 2024-10-07

I've been banging together an 6502-CPU opcode assembler to get away from
homoeostasis while trying to fix I/O.  eof() has to be delayed with the
CBM KERNAL.  The idea to was to write a bytecompiler for fox ache!

Halfway through writing a char number tree based tokenizer I realized
that an assembler wouldn't need a tokenizer at all.  Let's take this
real-world example, showing how READ would change only few lines:

~~~asm
cons?ax:
    sta p
    stx ,(++ p)     ; stx (unquote (++ p))
cons?p:
    lda ,(++ p)     ; lda (unquote (++ p))
    beq no
    ldy #0
    lda (p),y       ; lda (p) (unquote y)
    and #TYPE_CONS
    beq no
    lda #,(lo t)    ; lda # (unquote (lo t))
    ldx #,(hi t)    ; ldx # (unquote (hi t))
    rts
no: tax
    rts
~~~

UNQUOTES that don't denote an indexed address mode are evaluated while
parsing the read expression.  The may span multiple lines.  Character
'#' is cut out of the heads of symbols.  Single quotes however need to
be checked for before something is READ or quotes at line ends cause
trouble wanting anything following as its argument.

~~~
    and #TYPE_CONS  ; and # TYPE_CONS
    lda #'A'        ; lda # 65
~~~

Symbols in expressions are replaced by label values if available.
During the first pass ONERROR helps ignoring missing forward references.
Then it's ignoring errors again until all instruction sizes (and branch
ranges) are known.  The last pass includes generating the opcodes and
operand bytes.

# 2024-10-04

RETURN is ignored in argument lists but I'm at it, hoping that this will
finish AUTOLOAD which works nicely otherwise.  Especially noteworthy is
its ability to expand missing macros in already running code.  Anyway:
The RETURN issue needs to be fixed to satisfaction.  With the everything
loading on demand properly and the debugger REPL stepping properly, a
v1.0.0 release is closing in.  Without a bytecode compiler, because
there still is a tutorial, manual and references to complete.

# 2024-09-27

I hate taking breaks from challenging and creative tasks that require
grand pictures.  So let me reboot: TUNIX Lisp is about educational
purpose in the first place.  What's there is not that playful toolbox it
has to be, and I'm equally tired and motivated, which is calling for a
break again.

After several attempts I'm doing the new kids' intro to Lisp on the
project's Github wiki.  Maybe it'll find its way into the "Garbage
Collected Manual".  Renaming classic function names to something that's
tangible, e.g. CARLIST to FIRSTS and CDRLIST to RESTS.  FIRST instead of
CAR and REST instead of CDR is also nice but combinations like CAAR and
CADR hit a spot either.  Will go suspend to recharge the batteries
faster.

...zzZZzz...

# 2024-09-25

TUNIX Lisp compiles for CP/M but I didn't get through the trouble of
setting up an emulator.  The generated Z80 code doesn't look any better
than the 6502 assembly.  Unless TUNIX Lisp is written in itself and
compiles itself that's as far as it gets.  Perhaps Oscar64 or llvm-mos
can do better but I have a bad feeling about code size.

# 2024-09-24

Actually I (tried) to relax for a week, letting things dangle, and
dreaming along.  That was about what the doctor ordered, says the body.

Am hacking along with compiling sets of functions, loading and running
only one compiler pass at a file, and using temporary files.  The CBM
versions unnerve with unexpected behaviour.  Looking ahead for the
progress.

# 2024-09-23

Using NCONC instead of APPEND in MAPCAN saved the day.

# 2024-09-16

The first version of the compiler is nearly complete but
untested.  Explains why FOLD-BLOCK misses some blocks.
Nothing exciting.  What's bothering me though is there seems
to be more heap required already than expected.  But when I
remember correctly I didn't expect more than one pass to fit
in from the start with the VIC-20 in mind.  Admittedly TUNIX
Lisp is undertested and not profiled at all.  A benchmark
should help adjust expectations and find design flaws – or
just things done wrong (also known as "bugs") or missing.  I
have a strong feeling that too many unused objects remain on
the heap.  Might be just the global pointer 'value' which
holds the result of the last evaluated expression.  Will
check when some benchmarking is available.

Bytecode functions will be symbols that have the bytecode in
their name plus an extra type flag.  RAWPTR has to allocate
the returned number first and assign the pointer value
afterwards because a GC run could make the value invalid due
to object relocation.  A new built-in SET-CHAR-AT is also
required.  It shouldn't trigger the GC, so that won't cause
problems.

And of course there has to be a bytecode interpreter.  Also
not rocket science and perhaps a thing that could be done in
assembly alongside a C version.  Take away the tests and the
compiler is less than 200 lines of code – the program will
have to stop on errors.  Weaving in the debugger is a bit of
a thrill thinking about.  Postponed for the sake of inner
peace.

Speaking of: I had an AI coding session today which went
execptionally well.  One reason for that is that GPT's can
capture the simple structure of Python code to satisfaction;
plugging together libraries to build AI with AI is
next-level inspiring.  Perfect for academics who cannot
code.  Now the use of the Lisp shell mode I've been carrying
around since 2007 is becoming clearer.

Am coming to terms with a shitload of projects that got
stalled recently and I'm nearly where I wanted to return to:
live and work in peace, with meaningm and to be a bit on top
of all things life.  That cold I caught can't phase me.  I
just know that I'm having one.

# 2024-09-15

I wasn't in the mood to code over the weekend.  Turned to
entertaining books instead, met complete foureigners and
wrote some TUNIX Lisp compiler documentation to clear the
path.  For the sake of educational purpose (including my
own) I'm trying to boot and explain the compiler in small,
and partially unusual, steps, like compiling the control
flow and still interpreting function calls.  I have a
warm fuzzy feeling about this version of the compiler,
although I have no idea what purspose other than putting
more contrast to separation of concerns to benefit learners
it might serve.  A warm fuzzy feeling is just enough for
me to get on.  It indicates spot-on intuition that might
inspire others - big league compiler design is creeping
in in an appetizing and digestible fasion if I'm lucky.
For now I'll save up the energy to explain this more in
detail in the docs.

# 2024-09-12

Even a most-simple compiler will work wonders on 6502-based
systems, where the editor is unusable and loading it is
taking over six minutes.  It would be of help if just the
control flow would be compiled, so all code would be in a
single array to jump around in.

So there's a new plan:

[x] Make \*MACROS\* an associative list.
[x] Expand ?, AND, OR.
[@] Expand BLOCK, RETURN, GO.
[ ] Inline function expressions.
[ ] Bytecode assembler.

Changing the macro list and starting the compiler macro
expansion was easy.  BLOCK could use a nap before.
Inlinined expressions will call new %PUSHARGS and %POPARGS
to save and restore argument symbol values.  The bytecode
assembler will just have to translate labels into offsets
and create a list containing the bytecodes.

Enough trouble for the weekend.  The bytecode interpreter
can be embedded into eval0() – but native code could also
be generated (probably taking too much memory).

# 2024-09-10

AUTOLOAD is fairly complete, but the editor is far too
slow, the debugger has a bug, and images aren't working
to satisfaction either.  If only the interpreter would be
a bit faster.  It looks like there's heavy processing when
going up and down.  Should be easy to track down.

# 2024-09-06

Implemented AUTOLOAD today within a bekloppie.  Does not
work with functions passed by arguments but goes well with
regular function and macro calls.  Now for the other way
around: throwing out procedure bodies when memory is low.

Am very happy with this release, v0.0.16.

# 2024-09-04

The editor can load files, re-using EDIT-LINE for the
filename prompt.  Feels like closing in on a real release
if only the manual wouldn't be 70 pages of just notes.

Since everything is rather slow, the editor will have to
to be saved as an image for regular use.  It's also time to
come up with an autoloader for missing procedures, and a
sensible way to remove unused code.  Probably by resetting
symbols in \*UNVIERSE\*, so they point to themselves.
Also, unnaming symbols and common subtree elemination (CSE)
will make lots of free heap.  But the latter has to be done
with a modern machine unless... some hash stack helps out
with finding subtrees.  There's also that list compression
that might be effective.  Enough promising things to dream
about.  For real.  Good night!

# 2024-09-02

I gave the terminal code a little rework.  The Unix part is
more or less untested.  The editor also has to run on Unix
terminals, so it can run on microcontrollers.  I also
implented APPEND as an optional built-in funciton, expecting
it to give MACROEXPAND a little boost.  And happy me, I also
removed some unnecessary use of the object stack.  Every
piece of code is dragging 8-bit targets down noticably.  I
ended up breaking the test runs and that is frustrating at
the end of the day.

I think my mind got numbed by some plant-based gnat
repellant, because since I'm not using it any more I'm not
waking up, feeling like I've been on a drinking binge, and I
can think clearer now.  Just for the record: I only do
coffee and tobacco and sugary things tend to rot away in
reach.  I'm an athletic disappointment to the doctors when
it comes to earning money, doing 50 miles on a bike without
getting exhausted.  That's what programming is for.

Now I'll go dream about a fantastic Lisp IDE for our beloved
machines.

# 2024-09-01

Two days passed and yesterday I woke up brain-dead.  One
more minute of programming and I would have qualified for
and official care level and free drool bib.  Great
opportunity to head out and inspect other things entirely
and to visit friends - mine aren't into the details of
computing and that's perfect for me.  I barely talk about
what I'm doing with them and if I'm away from the machine,
that's usually because I'm fed up.

Implementing NTHCDR and SUBSEQ as C functions built into the
interpreter, as well as adding OUTLIM, improved performance
as expected.  Similarly expected, SUBSEQ allocates too much,
so garbage collection is triggered too often.  Unfortunately
native CBM console output isn't known for its snappiness
either.  When outputting a list of chars, they'll have to be
written to The Buffer first or the overhead of calling out()
is weighing in too heavily.  But whole lines shouldn't be
updated for each keypress in the first place, nor should the
whole screen be for up and down movements, unless it needs
to be scrolled.  cc65's conio library doesn't support
scrolling any way.

Another thing on the list is replacing SUBSEQ by the new
function CUT-AT which splits a list at a position and
returns the cut-off tail.  That actually removes two SUBSEQs
for inserting chars or lines.

# 2024-08-29

Oh dude, is the editor slow, or what?  The need for more
built-in functions is pressing big time.  Instead of SUBSEQ,
a C version of NTHCDR and an OUT with a length limit should
work the miracle.  SUBSEQ makes copies and we don't want the
GC to kick in regularly, especially not for displaying text
alone.

~~~lisp
(outlim line-width)
(out (nthcdr xstart line))
~~~

OUTLIM just has to set a countdown for OUT (which defaults
to -1 if there's no limit).  Implementing NTHCDR as a built-
in is definitely a no-brainer as well.
Tomorrow.  It's been an action-loaded day and I'm desperate
for a complete chunk of healthy sleep, including sweet
dreams.

# 2024-08-29

Release v0.0.12.  Starting the first app, the integrated
text editor in this case, always comes with early
disappointments and writing more tests instead.  But it's
taking shape.  I don't care that it is slow.  The reason for
that is that the current line is a list of characters and
functions like NTHCDR and SUBSEQ are implemented in Lisp
which can run around 300 expressions per second.
Implementing those, and perhaps a couple more,  in C could
take the edge off well enough to make the editor usable.
Actually everything that has to walk a line should be done
in C before a bytecode compiler is around to help out.

I dearly missed to ability to walk up and down the stack,
or to just list the call stack.  The tag stack might be
helping out implementing such thing rapidly.  No stress.
I planned to grow old in peace.

# 2024-08-24

Release v0.0.8.  For Sunday entertainment I'll try to solder
one of those SD2IEC drives laying around.  Since the VIC has
an Ultimem expansion that's just the right time to get
'src/sys/boot' to do more than just print a menu and tell
which of the eight boot banks contain any data.  As it's
written in C.  Now I'm asking myself why not use Lisp for
that.  Stripping down most features probably isn't enough.
But then again exomizer to the rescue...

Isn't it amazing how I manage to stretch doing things that
could be done since yesterday by being a bit lazier?
OK, so let's be serious.

# 2024-08-24

cc65 didn't have no bug as far as I can see and that's a
relief.

Now it's time to get serious and make use of the true powers
of Lisp.  So all tests moved into an own file and manual
loading of dependencies isn't done any more.  Am thinking
code-walking and automatic loading.

The user experience is much more favourable.  The loading
time to make the initial environment image has been reduced
to less than a sixth.  Thus: release of v0.0.8.

And off we shoot...

# 2024-08-24

We're now at release v0.0.6.  Unfortunately a bug in
cc65's peephole optimizations prohibits to give the
Lisp a little boost.  Today wasn't the day to spend
a couple of hours on that.  The Lisp environment grew
to over 1,300 lines.  Was a little suprised how much
code accumulated already – and the manual could use
some gaps filled.  I don't like it's style.  Actually
it should be great to teach kids programming.

# 2024-08-22

Phew!  Release v0.0.4 fixes a lot of things.  Especially
I/O.  Enjoy!

Guess I deserve a round of ice cream right away! :p

---8<------8<------8<------8<------8<------8<---

I shared with a wasp which seemed to have had several
orgasms eating it.  It, too, must have been older.

Now for the hard part.  The debugger is off-putting,
especially if the sugar flash fades.  What's this?
(Calling MAPCAN with a function only.)

~~~lisp
DEBUGGER 1:
Error #2: Missing args: (first . rest)
In mapcan (f . l):
((apply append >>>(apply mapcar f l)<<<))
~~~

It is APPEND with argument defintion (first . rest), which
is wrong in my mind.  Let's see what SBCL says:

~~~lisp
(append)
nil
~~~

Cool.  There's the relief of having guessed right.  But,
since the call has been made inside APPLYm and it's last
argument is still marked as bein g highlighted, the info is
confusing.  Before APPEND can be fixed, I'll have to make
sure that the debugger won't steal time acting like this.
Essentially we want something like:

~~~lisp
DEBUGGER 1:
Error #2: APPEND: Missing args: (first . rest), got: nil
In mapcan (f . l):
((apply append (apply mapcar f l)))
~~~

Accordingly I'll make sure that the highlighting is cleared
after the last argument to a function has been evaluated.
One moment... and there is the blues: EVAL pushes and pops
the highlighting marker onto the GC stack.  That should be
OK.  I'm tired already.  It's late and the classical music
by Schubert („Der Tod und das Mädchen“, Das Busch-Quartett),
I left on, because I didn't care really, is getting on my
nerves, racing towards its finishing peak.  NnnnnnnnngAh!
Thanks, Schubert.  That's better!  I'll go explore the
virtues of horizontal mind mapping, leaving a throw-away
note: "Show name of called function by APPLY and discard
highlighting."
But I'll APPEND a quick fix.

# 2024-08-21

Unix input channel works, CBM doesn't.  It's been a long
day and, as usual, utterly frustrating to have to give it
a break.  Found a Rasperry Pi Pico + magazine in the
mailbox (dead tree version) but when it comes to install
the command-line development tools on Linux, I'm as smart
as last night.  It mentioned that there is some "ulisp"
right in the front of the magaizine but no more info on
that either.  Not very motivating either.

# 2024-08-20

Thinking about the editor, alongside just messing up
the Lisp's intestines and forgetting that it has its own
debugger so gdb can be put aside once in a while, the idea
of compressing lines returned.  With TUNIX Lisp, the symbol
look-up can help with that by splitting up lines into chunks
of spaces, punctuation, control sequences and the rest of
characters.  Calls for a new function called
GROUP-BY-PREDICATES:

~~~lisp
(group-by-predicates x whitespace? punctuation? ctrl?)
~~~

Also mathing the case when no predicate on the list rings.
That one either has to be built-in, compiled, somehow be
running in the background when waiting for keypresses, or
in an extra pass.

Another idea that is coming closer to free heap is to
replace named symbols by unnamed ones.  Optionally,
procedure names and arguments may remain untouched.

# 2024-08-20

I can't stop thinking about giving the editor cut and past
and undo - of course without letting it grow much larger.
That's probably because I got woken up in the middle of the
night.  The I/O needs separate status info for each channel,
of course.  It wasn't fun to debug.  Back to bed.

# 2024-08-19

Did v0.0.3 yesterday, mainly because there was help to
get compiled with clang.

READ has been reworked and can also read negative numbers,
but I've messed up EOF.  On all platforms.  Will continue
work tomorrow evening perhaps.  Could use a paid job.

The sketched text editor occupies 3.5K heap so far which
leaves me a bit baffled.  Could be running on the VIC-20
with no "tricks" like overlays.  Now for a night worth a
pause.

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
