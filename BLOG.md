TUNIX blog
==========

# 2024-05-23

The Lisp interpreter's GC passes stress tests, the scariest
checkpoint. :)  NIL and 0 are now equal which is an unusual
thing with Lisps.  I don't know any dialect going down that
road so it might be dangerous.  It's less code and a bit
faster.  Strings are required and they are already there in
the shape of symbols.  Having strings without a symbol value
would add overhead and eat away on heap size.  The CBM
KERNAL way of doing I/O looks like a good match for this.
With a REPL no-one should be offended.  It's play time with
funny demo/testing code. <:)

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

# 2024-05-14 20:11 (smk)

Small-C has been merged in, but you won't recognize it any
more soon: with the plan to use a Lisp interpreter to
generate C compiler code, it'll be a whole new compiler.
Am not feeling very good about writing an interpreter or
compiler again unless there's plan with medium resolution
and snappy time spans.

# 2024-05-08 16:48 (smk)

exomizer-3.0.2 and oscar64 have been added.  Will try to do
an oscar64-compiled kernel and exomizer is used all over the
place anyhow.

# 2024-05-08 01:18 (smk)

A lot of work on the kernel hasn't been blogged about.
More happened to the "Bielefeld DB" which will be the
system's embedded key/value database with b-tree index and
secondary storage to free main memory.

There has to be a full-scree console or working with TUNIX
and text editing will be a pain up main street.  At least
the width of a terminal has to be 60 chars.

# 2024-04-30 19:41 (smk)

Cleaned up the syscall request code a bit.  The CSV format
seems to be just right for BASIC programs but binary formats
would also be appreciated.

I'm little brain-dead because of my speed-head neighbours
but I've found some awesome ear plugs in the shop.  This
will be interesting to watch.

There's still a weird memory bank bug.  To be handled
tomorrow... 8)

# 2024-04-26 23:16 (smk)

Added ownership checks of extended memory banks but nothing
new so far.

# 2024-04-24 02:34 (smk)

Can mass fork, but only up to 12 processes.  There're isues
with memory allocation.  Am happy enough with how it got at
this time of day.

Good night!

# 2024-04-23 16:08 (smk)

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

# 2024-04-21 16:59 (smk)

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
