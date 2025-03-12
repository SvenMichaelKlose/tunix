TUNIX Quick Manual
==================

TUNIX is a KERNAL extension for the Commodore VIC-20 with
UltiMem expansion that adds pre-emptive multi-tasking,
loadable drivers and recover-on-reset.

# Status

See the [list of things to do](TODO.md) for the current
project state.

# Run in VICE

If you don't have an UltiMem image ready, you can create an empty one in the Unix shell like this:

~~~sh
dd if=/dev/zero of=empty-ultimem.img bs=1M count=8
~~~

Start the VICE emulator in this directory:

~~~sh
xvic -ultimem empty-ultimem.img -virtualdev8
~~~

In BASIC, configure a +37K expansion:

~~~BASIC
POKE 40945,63 : POKE 40946,255 : POKE 40944,64
~~~

Now you can start the kernel:

~~~BASIC
LOAD "TUNIX",8
~~~


