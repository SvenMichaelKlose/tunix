TUNIX
=====

# Introduction

Multi-tasking KERNAL I/O extension for
the Commodore VIC-20 with UltiMem
expansion.  It's the foundation of the
INGLE operating system distribution.

## Wanted Features

* Up to +35K RAM for all processes.
* Portable UNIX-style KERNAL I/O API.
* Pre-emptive multi-tasking.
* Loadable drivers.
* Resume on reset for crashed apps.
* Networking.
* Remote computing.

## Memory

Except for the IO23 area, which is
reserved for TUNIX and drivers, all
memory is available to programs.

## Performance

Thanks to the UltiMem expansion, TUNIX
is blazingly fast as most of process
switching is done in hardware, allowing
apps to access up to 40K RAM (IO23 area
not included).

### System calls

The most expensive operation is the
fork() system call, which can take up to
355ms to complete since the address
space of the forked process need to be
copied completely.
Switching one native VIC application to
another takes 21ms, from a native to a
TUNIX app (or vice versa) 7ms and a
swith from a TUNIX app to another TUNIX
app brills at 0.3ms to 10ms.  The main
difference making TUNIX apps faster is
that they do not access screen memory,
which is handled by the console driver
exclusively.

Time-consuming task switches aren't much
of a factor as native apps are not
multi-tasked and run only when they are
active on the console. TUNIX apps can
still run alongside unless that has been
disabled (e.g. to run programs that take
over the machine).

## I/O

Calling a driver takes 0.27ms.  This
leaves us with at least 17.7s overhead
for transmitting or receiving 64K
characters via device drivers.

The TUNIX system call driver's overhead
is limited to parsing and dispatching
commands.  Resource allocation and
deallocation uses fast deque operations
at constant execution times.

## Educational Purpose

By providing a platform that is both
historically interesting and technically
challenging, TUNIX encourages the
exploration of computing principles,
hands-on learning, and the joy of
bringing new capabilities to old
hardware, all within the context of a
community-driven project:

* Understanding Operating Systems:
  TUNIX introduces Unix-like
  functionalities to a platform that was
  not originally designed for such
  complexity.  This offers learners a
  unique opportunity to understand the
  components of an operating system,
  such as multitasking, memory
  management, and file systems, in a
  simplified and accessible environment.
* Programming Fundamentals:
  The project’s development and its
  support for programming in
  environments like BASIC, ANSI-C, and
  potentially others provide a practical
  context for learning programming
  fundamentals.  It allows students to
  experience the development cycle, from
  writing and debugging code to
  integrating with an operating system.
* Historical Insights:
  By enhancing a vintage computing
  platform, TUNIX provides insights
  into the evolution of computing
  technology, offering a hands-on
  history lesson.  It shows how
  limitations of early computers were
  overcome and how these systems laid
  the groundwork for modern computing
  paradigms.
* Principles of Unix:
  The Unix philosophy emphasizes
  simplicity, modularity, and
  reusability. TUNIX makes these
  principles tangible for learners,
  allowing them to explore how small,
  focused programs can work together to
  perform complex tasks.  This can
  deepen understanding of software
  design and architecture.
* Low-Level Computing Concepts:
  TUNIX offers exposure to low-level
  computing concepts, including assembly
  language programming and direct
  hardware interaction, within a
  controlled and understandable
  environment.  This can demystify how
  software interfaces with hardware, an
  important area of computer science
  education.
* Problem Solving and Innovation:
  Given the VIC-20’s hardware
  limitations, TUNIX challenges
  developers and learners to think
  creatively about resource management,
  optimization, and functionality
  implementation.  This can foster
  problem-solving skills and innovative
  thinking, valuable competencies in any
  technical field.
* Collaboration and Open Source Contribution:
  If structured as an open-source
  project, TUNIX provides a platform for
  collaborative learning and
  contribution.  Participants can learn
  from each other, share knowledge, and
  contribute to a collective project,
  gaining experience in version control,
  code review, and documentation in the
  process.
* Cross-Disciplinary Learning:
  TUNIX bridges computer science with
  electrical engineering, history, and
  even elements of graphic design and
  user experience.  This multidiscipli-
  nary approach can appeal to a wide
  range of learners, encouraging cross-
  pollination of ideas and techniques.


# Using TUNIX

TUNIX' user interface allows you to
switch between consoles you either
created with a fresh instance of BASIC,
or cloned with something running, and
of course to kill or restart a console.
The menu to do that opens up when
starting TUNIX or by pressing and
releasing the Commodore key.

## The Console Menu

The console menu display a list of
console slots and their ID numbers.
10 slots are available.
It is controlled by the keyboard.
Numbers 0-9 select a console.  RETURN
sitches to it.  Other commands for
selected consoles are:

* "C": Clone console.
* "Mi": Move console to slot numner 'i'.
* "R": Restart BASIC in selected 
* "X": Remove console from slot.

# Programming for TUNIX

TUNIX is hijacking the standard KERNAL
I/O to run and provide its services.
Unlike Unixoids the KERNAL does not
provide default LFNs for standard I/O.
Instead, apps reset the current LFN-
derived device pair to the keyboard and
screen device using the CLRCN system
call.  To make up for this, a driver
allows to connect LFNs to devices, so a
process' standard I/O can be pipelined.

# CBM & TUNIX KERNAL I/O

This section describes the behaviour of
the original KERNAL functions, along
with the slightly different behaviour of
TUNIX to support a multi-tasking envi-
ronment better.

All I/O call performance is reduced
by about 1ms per operation in addtion to
the operation itself.

## General behaviour

Functions that return with an error code
do so with the carry flag set and an
error code in the accumulator (A).

Pointers passed in Y and X registers
have the low byte in the X register and
the high byte in the Y register
respectively.

### SETNAM - Set file name.

This routine is used to set the filename
for the OPEN, SAVE, or LOAD system
calls.  A pointer to the filename is
expected in the YX register pair (X is
the low byte) and the length of the in
A.

SETNAM never returns with an error.

### SETLFS - Set parameters.

SETLFS will set the logical file number
(A), device number (X), and secondary
address (Y) for OPEN, LOAD or SAVE.

The logical file numner (or LFN for
short) used with OPEN will be translated
to the device number by CHKIN/CKOUT
for BASIN, BSOUT or GETIN.

The device number may range from 0 to
255 (30 with standard KERNAL I/O). These
are the commonly used device numbers:

~~~
0:     Keyboard
1:     Tape
2:     RS-232
3:     Screen
4+5:   Printers
6+7:   Plotters
8-12:  Disk drives
13-30: user defined
--
31:    TUNIX system calls
~~~

The secondary (SA) is sent to IEC
devices.  The c1541 DOS uses it to
distinguish between open files like the
LFN does.  That's why the SA usually has
the same value as the LFN in human-
readable code.

SETLFS never returns with an error.

## OPEN - Open logical file.

OPEN assigns the device and SA to the
LFN that was specified when calling
SETLFS.  A filename must have been
set using SETNAM.

The LFN of a successfully opened file
can be used with CHKIN and CKOUT to
set the input and output device for
BASIN, BSOUT and GETIN.

TUNIX copies the filename to the IO area
(255 byte maximum) and translates the
LFN to a GLFN before calling OPEN.

OPEN clears the STATUS byte.  On error,
it returns with the carry flag set and
one of these codes in A:

* 1: Too many files.
* 2: File already open.
* 4: File not found. Tape device only.
* 5: Device not present.
* 6: Not an input file.
     Also if logical file number is 0.
* 9: Illegal device number.
     Also if tape buffer is below $0200.

## CHKIN - Set input device by LFN.

Takes the LFN in X and sets the input
device of BASIN and GETIN to the one
assigned to that LFN when OPEN was
called.

One of these error codes may be
returned:

* 3: file not open
* 5: device not present
* 6: file is not an input file

## CHKOUT - Set output device by LFN.

Takes the LFN in X and sets the output
device of BASIN and GETIN to the one
assigned to that LFN when OPEN was
called.

One of these error codes may be
returned:

* 3: file not open
* 5: device not present
* 7: file is not an output file

## BASIN - Input character from channel

Reads a byte from the device selected by
CHKOUT, CLRCN or CLALL and returns it in
A.

When reading from the KERNAL keyboard,
the input is buffered up to a length of
80 before or a carriage return is typed.
Meanwhile, the cursor is blinking.

On error this routine returns with the
carry flag set and additional flags in
the STATUS zeropage locate (also see the
READST system call).

## GETIN - Input character from channel

Same as BASIN but returning 0 with the
keyboard if no input is available.  E

## BSOUT - output a character to channel

Writes a byte to the device selected by
CHKOUT, CLRCN or CLALL.

NOTE: Care must be taken when using
routine to send data to a serial device
since data will be sent to all open
output channels on the bus.  Unless this
is desired, all open output channels on
the serial bus other than the actually
intended destination channel must be
closed by a call to CLOSE before.

On error BSOUT returns with the carry
flag set and additional flags in the
STATUS zeropage location (also see the
READST system call).

The Y register is not destroyed.

## CLRCN - Close default input and output files

Set the the input device to the key-
board (0) and the output device to the
screen (3), the only means to access
standard I/O.

CLRCN never returns with an error.

## CLOSE - Close logical file

Closes the LFN passed A.  Never returns
with an error.

## CLALL - close all channels and files

When KERNAL version is called, the
pointers into the open file table are
reset, closing all files.  Also the
I/O channels selected with CHKIN/CKOUT
are reset to the console, like CLRCN
does.

With TUNIX, CLALL does not just drop all
open files like the standard KERNAL
but calls CLOSE on each driver, and the
I/O channels are reset as well.

CLALL never returns with an error.

## LOAD - load RAM from a device

Loads a block of bytes to memory and
can also be used to verify them with a
block in memory withot affecting it (by
setting A to 1 instead of 0).

SETNAM and SETLFS must be called
beforehand.  The LFN used with SETLFS
is ignored.

The destination address of the block is
specified by either the first bytes of
the file (if the SA is not 0).  In that
case the address is taken from the YX
register pair (with X as the low byte).
YX will return the address of the byte
following the end of the block.

LOAD returns the same error codes as
OPEN:

* 1: Too many files.
* 2: File already open.
* 4: File not found. Tape device only.
* 5: Device not present.
* 6: Not an input file.
     Also if device number is 0
     (keyboard).
* 9: Illegal device number.
     Also if tape buffer is below $0200.

## SAVE - save RAM to a device

Mirroring LOAD, this saves a block of
memory to a device.  SETNAM and SETLFS
must have been called beforehand.  The
LFN used with SETLFN is ignored.

SAVE expects the zeropage location of
the address of the memory block in A.
The YX register contains the size of the
memory block (X is the low byte).

SAVE may return one of these error codes
if the carry flag is set:

* 1: Too many files.
* 2: File already open.
* 4: File not found. Tape device only.
* 5: Device not present.
* 7: Not an output file.
     Also if device number is 3
     (screen).
* 9: Illegal device number.
     Also if tape buffer is below $0200.

# System calls

System calls are performed via opening
a file on device #31.  The first
character of the filename denotes the
command group, the second the command in
that group.  Argument bytes may follow.

After sending a system call via OPEN,
an error code may be read using BASIN.
(Calling CHKIN is not required.)  It may
followed by return values.
On assembly level the carry flag tells
if an error occured (carry set with code
in accumulator instead of the return
value).

~~~C
// Get process ID.
open (31, 31, 0, "PI");
pid = basin ();
~~~

Most system calls return an error code:

~~~C
// Put process 1 to sleep.
open (31, 31, 0, "PS\x01");
err = basin ();
if (err)
    printf ("Process 1 already asleep.");
~~~

Some return an error code and return
values:

~~~C
// Fork
open (31, 31, 0, "PF");
err = basin ();
if (err)
    error ();
id = basin ();
if (!id)
    child_stuff ();
~~~

~~~C
// Get process IDs, flags and names.
open (31, 31, 0, "PL");
while (n = basin ()) {
    if (readst ())
        break;
    printf ("%d %d %s\n", id, f, n);
}
printf ("\n");
~~~

## List functions

The functions return a list of comma-
separated values, terminated by
newlines. It is introduced by a comma-
separated list of field names that never
contain commas themselves.  The order
and number of fields may vary.

Fields containing strings start and end
with double quotes (").  Inside double
quotes have to be quadruple quotes ("").

An example:

~~~
ID,PID,NAME
0,1,"INIT"
1,0,"CONSOLE"
1,0,"RAMDISK"
1,0,"C1541"
1,0,"IP65"
2,0,"BASIC"
3,0,"SOMETHING ""COOL"""
~~~

## General

### "": Schedule

A filename of length 0.
Gives TUNIX the opportunity to switch to
the next process without doing anything
else.  Like the other system calls it
may switch unless in single-tasking mode
(command "SS").

### "G$": List general infomation

Unlike the other list functions this
one returns a list of comma-separated
key/value pairs.  These are the keys:

1. "NPROCS": Number of processes.
2. "MPROCS": Max. number of processes.
3. "NDRVS": Number of drivers.
4. "MDRVS": Max. number of drivers.
5. "NIOPAGES": Number of IO pages.
6. "UIOPAGES": Number of used IO pages.

### "GM": Multi-task

Enables task-switching via interrupt.
The default.  Task switches are
scheduled after all system calls.

Processes that did not free the screen
will have that switched in for the time
they are running.

### "GS": Single-task

Disables task-switching.  Should be used
sparingly.

### "GX": Shut down

Closes all files, kills all processes
and returns to plain BASIC.

## Processes

### "P$": Process list

Returns a comma-separated value list
(CSV), headed by field names.
A process returns these fields:

1. ID: Process ID.
2. PID: Parent rocess ID.
3. NAME: Pathname if the executable.
4. FLAGS: (R)running, (S) sleeping,
   (Z)ombie
5. MEM: Allocated memory in bytes.
6. IOP: Number of allocated IO pages.

~~~
ID,PID,NAME,FLAGS,MEM,IOP
0,0,"INIT",S,49152,0
1,0,"CONSOLE",R,49152,0
2,1,"BASIC",R,49152,0
~~~

### "P": Process ID

Returns current process ID.

### "PI": Process info

Discloses all internal information about
a process.  Returns a list of double-
colon-separated key/value pairs, the
keys being composed of upper case
letters and digits only.  List values
are comma-separated.

Currently returned fields:

* "ID": Process ID.
* "FLAGS": RSZ
* "EXITCODE": Exit code (0 by default).
* "WAITING": Processes waiting for exit.
* "MEMORY": Banks assigned to shadow
  RAM123, proc RAM123, IO23, BLK1, BLK2,
  BLK3 & BLK5.
* "BANKS": List of extended memory
  banks.
* "STACK": Last saved tack pointer.
* "LFNS": Used LFNs.
* "GLFNS": Analoguous global LFNs.
* "IOPAGES": Allocated IO pages.
* "DRIVERS": Registered drivers.
* "DEVICES": Device drivers.
* "SIGNALS": Signals to receive.

### "PNp<new name>": Process name

Sets the name of process 'p' (zero-
terminated string) if it follows the
command.  Returns the current name if
none was set.

### "PF": Fork

Makes a copy of the current process
with a new ID that is returned to the
parent.  For the child an ID of 0 is
returned.  Open files are shared
by both processes and be closed by
both.

Extended memory banks are inherited but
not those that were banked in when
the fork was initiated.

A fork takes 25ms.

### "PEname and args": Execute

Replaces the current process by an
executable in TUNIX format.  But fear
not:  'runprg' and 'basic' will also
launch native programs.

### "PXc": Exit

Exits the current process with an exit
code.  It remains on the process list
until its parent exits or returns from a
wait() on it.

### "PWp": Wait

Waits for a process to exit and returns
its exit code.

### "PKp": Kill

Exits any process with exit code 255.

### "PSp": Suspend

Moves process to the sleeping list so
that it won't be executed after the
next task switch.

### "PRp": Resume

Moves a process to the running list.

## Extended Memory

Allocation and deallocation of extended
memory banks happens fast at a constant
speed.

### "M$": Get memory information

1. "USED": Total extended memory in
   bytes.
2. "FREE": Used extended memory in
   bytes.

## "MA": Allocate a bank

Allocates a bank and returns its number.
Returns with an error when out of
memory.

## "MFb": Free a bank

Frees a bank.

## Drivers

Drivers process KERNAL I/O calls for a
specific device.  They operate in the
context of the calling program but in
their very own address space.  The IO23
area is used as a hub to cross address
spaces.

Processes may register one or more
KERNAL I/O vector tables to be used
with devices.

When a driver functions is called, TUNIX
only banks in BLK1 of the driver process
to make up for the low performance of
the hardware.  If the presence of
zeropage locations or additional banks
is required, the driver must set missing
banks and/or restore used zeropage
locations itself.

TUNIX translates logical file names to
global logical file numners (GLFNs), so
they won't collide with those used by
other processes unless they are shared
after a fork().
An LFN is always translated to the same GLFN until the process exits.

### "DL": Driver list

Returns a comma-separated value list,
headed by field names.

TUNIX returns these fields:

* ID: Process ID.
* PID: Parent rocess ID.
* NAME: Pathname if the executable.
* FLAGS: (R)running, (S) sleeping,
  (Z)ombie
* MEM: Allocated memory in bytes.
* IOP: Number of allocated IO pages.

Example output:

~~~
ID,PID,NAME,FLAGS,MEM,IOP
0,0,INIT,S,49152,0
1,0,CONSOLE,R,49152,0
2,1,BASIC,R,49152,0
~~~

### "DRdvv": Register

Registers a KERNAL I/O vector table for
a device.  Global to all processes.  It
unregisters when the driver exits.

### "DVd": Get driver of device

Used to overlay vectors by a another
driver.

### "DA" Allocate IO page

Returns the high byte of the page which
will be present in all processes.
Used to run interrupt handlers.
Needs to be commited (to be portable)
before use.

### "DCp": Commit IO page

Copies the page from the current pro-
cess to all others.

### "DFp": Free IO page

Frees an IO page.

## Signals

Signals are asynchronuous calls of
other process' functions.  These
functions, called _handlers_,
can be _registered_ by _signal type_ (a
byte value) or default TUNIX handlers
are called instead when a signal is
_delivered_.  The exact handler to call
is determined at the time it is sent to
allow unregistering a handler, to be
able to call the default action as a
fallback.

Signal type values can be chosen freely
as long as they are not used by the
TUNIX system itself.  A byte of payload
can be sent as well as the source of a
signal is not tracked to allow post-
mortem delivery (with the process ID of
the sender gone).

Pending signals are queued so low-level
interrupt handlers can send them with
little overhead.  Sending is cancelled
if a signal of the same type is already
on the queue.  It is not check if the
process provides a handler for the
signal type.  The signal is discarded on
delivery when the handler is missing.
This allows to send signals via
networks.

## System calls

"SStp": Signal

"SRthh": Register handler
"SUt": Unregister handler

## Internals

Internally, signals are tracked per
destination process.
