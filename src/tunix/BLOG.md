TUNIX blog
==========

# 2024-04-23 16:08

Although one of the main things on my
agenda with this project is to improve
my bad debugging skills.

Some procdata seems to get destroyed.
Either by a bug in the list/deque macros
or by whatever else that has gone rogue.
The basic deque tests could use some
more checks to really cover all details.

To detect off-scope memory writes I was
just thinking of a checksummer with
sitchable configuratios that contain
the memory area location and sizem and
the valid checksum for that area.  When
leaving a particular code section the
checksum can be created and checked for
validity when the section, (the only one
responsible for modifying that area) is
entered again.  When developing on a
TUNIX this is invaluable.

# 2024-04-21 16:59

It just appeared to me that having a
separate kernal stack might be too much
of a big deal to not implements,
although I'm not sure why yet.  The
kernel is currently more of a regular
library with a strange interface to
applications but banking.  I really
don't see no problems at the moment.

Looks like some basic data structures
are going rogue.  Did a two day break
to start (s)porting the Small-C compiler
to Commodore 8-bitters.  That's extreme
fun.  Also found a book about systems
programming which I didn't even know I
own.  Bliss...  Oh, shit!  I'm a nerd.
