Iingle compendium
=================

# Foreword

The Commodore VIC-20 is a highly im-
practical computer when it comes to
user-friendliness.  All programs are
stand-alone and usually one exits them
with a hearty reset to ithen engage in
yet another lengthy routine to launch a
program.  Text editing is enough of a
pain to get back to a modern machine.
It's impossible to do another thing in
parallel when a program takes its time.
Not even text editing.

With Ingle, Jim Brain's UltiMem ex-
pansion and an SD2IEC drive you are
getting warped into the world of UNIX
where BASIC is just one of many appli-
cations that can run in parallel.
You can switch between full-screen
terminals or the Desktop to have them in
windows.  A reset gets the system back
up at an instant.  A ROM and RAM file
system accelerate things further.

# Overview

Ingle extends the Commodore VIC-20
by multi-tasking, a console with many
terminals and a windowed graphical user
interface (the 'desktop') everyone can
write applications for.

Desktop apps include a simple text pro-
cessor, spread sheet and calendar -
enough for basic office work.

Last but not least Ingle can connect to
the Internet via serial line and IPv4.

# Core

The core[^2] is a layer between the
Commodore KERNAL and programs.  It
allows several processes to run in
parallel, to share devices and to create
virtual devices to communicate with each
other through multi-tasking.  The core
itself provides its services through a
virtual devices which makes it easy to
access with any programming language.

[^1]: Usually it would have been
called the 'kernel' if there wasn't the
CBM KERNAL already.

## Virtual device manager

This is the heart of the core.  It is
redirecting the KERNAL vectors to itself
and calls registered drivers or the
original KERNAL functions.

This is also where multi-tasking has to
be performed.  And it's dead simple: all
reads and writes to devices are
buffered.  A process switch is performed
as soon as one of those buffers runs out
of data.  Each process gets a turn.  A
driver can put a process to sleep so it
won't waste CPU time polling for data.
Programs that allow it, can also get
hooked up via interrupts to force task
switches.

Drivers are programs that register them-
selves as drivers with a set of I/O
vectors and then stay resident in
memory.  After that the driver is
available to all processes.
Every driver (and process) receives its
own set of logical file names, so it is
easy to track per-connection state.

## Process management

Ingle has the legendary fork() function
to run a copy of the current process
which in turn might replace itself with
another process using execute().  There
is no other way for creating processes
except when the core launches the first
one.

After a fork, the new child process
shares the same open files as its
parent.  Standards streams (keyboard,
screen) can be replaced before running
execute().

Under the hood execute() got a little
complicated because it has to
differentiate between regular programs
and those programs made for INGLE.

## System boot

The core is stored in and
automatically started from bank 0
of the Flash ROM along with INGLE and
most wanted applications on the ROM
file system.

The core boots by loading and running
the init batch script 'init' on the ROM
file system.  It contains a list of
drivers and programs to run one after
another.  One can reduce the list by
drivers that are of no use for example.
This can save a lot of time.

Usually the last item of the init
script is the 'desktop'.  (It can be
started on the command-line as well.)

# Console

The console provides eight screens for
terminals one can switch between by
pressing CTRL together with a function
key.  Unless a user name and password
has been configured a shell is opened
for each newly opened console.  F8
however is reserved for the Desktop.
It is built into the desktop.

# Terminals

Ingles terminal is VT-125 compatible (as
far as monochrome output is concerned)
and emulates Tektronix and ReGIS
commands for graphics.  Their resolution
is 40x24 characters or 160x196 pixels.
Fonts of variable width and height,
bitmap images can be rendered and
pointer input is supported, too.

# Desktop

The desktop provides a windowed version
of the terminal embedded into a window
manager with app menu and a file
selector box that can be launched by
programs via their terminals.

## Apps



# Command-line programs

All programs that ship with Ingle come
with manuals in Markdown format, so they
don't get too much attention here.

# Lisp

INGLE uses the AttoLisp interpreter to
process macros.  It uses extended RAM
but has no garbage collection.

# Appendix

## Terminal

### Key codes
### ASCII control codes
### Escape sequences
### Tektronix codes
### ReGIS codes

## Devices

* 0: keyboard
* 1: tape
* 2: rs232
* 3: screen
* 4: printer
* 5: vprinter
* 6: plotter
* 7: vplotter
* 8: SD2IEC
* 9: c1541
* 10: unused
* 11: unused
* 12: UltiFS ROM
* 13: UltiFS RAM
* 29: IPv4
* 30: Desktop
* 31: Ingle core

## Core device

### 15: Command channel
#### Process management
#### Virtual devices
#### Extended memory
