# Roadmap for v3 (TUNIX kernel)

This bootloader should do every Ultimem user a huge favour
and not change anything on the Flash ROM by default but
allow to deviate from what's already there.  The 'unused'
button on the expansion might do the trick.[^others]

[^others]:
  I don't know if others are using it already.  As far as I
  know most bootloaders don't care about their competition.

# Desired functions

## Configuratible

Selecting a boot bank or a ROM image on a filesystem for all
upcoming boots is highly desirable.  Having to re-select a
particular bootbank every time the machine was reset during
a night of hardcore gaming is time wasted.  The same is true
when developing apps, with VFORTH for example.

And while we're at it: autostarting a program that resides
on disk would also be highly advantageous.  But there is a
catch: all these selections require a memory configuration.
And there're still machines with no disk drive at all, so
it would be nice if the settings could be stored on Flash
ROM as well.  If you haven't been pissed of by coding CBM
I/O already, now's the time.
