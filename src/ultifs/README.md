UltiFS KERNAL extension
=======================

Start with BLK1, BLK2, BLK3 and IO1,2 enabled.

# Anatomy

UltiFS has been written in C mostly.
On launch it copies itself to RAM banks 117 (BLK1) to
119 (BLK3), modifies the KERNALs I/O vectors to point
at a wedge at $9800 and does a BASIC cold start.  The
primary wedge is responsible for banking the secondary
wedge in to BLK1.  It checks if the device number is
12.  If it's not, it returns to the primary wedge to
bank the old BLK1 back in and to continue with
whatever procedure the former KERNAL vector pointed to.
Otherwise BLK2 and BLK3 of the UltiFS is banked in, the
zeropage is saved and the UltiFS procedure in question
(OPEN, CLOSE, BSIN, etc.) is invoked.  When it has
finished, the saved (and probably modified zeropage) is
copied back over the original, BLK1 to BLK3 are being
restored and the registers and flags are set before
returning to the caller.

# KERNAL I/O functions

(Based on descriptions by Lee Davison and Simon Rowe.
See src/vm for the full ROM disassembly.)

This section describes the behaviour of the original
procedures UltiFS is trying to comply to.  Please let
the author(s) know should you notice deviations.

## General behaviour

Functions that return with an error code do so with the
carry flag set and an error code in the accumulator.
Pointers passed in Y and X registers have the low byte
in the X register and the high byte in the Y register
respectively.

## OPEN - Open logical file.

Once the logical file is set up it can be used for
input/output operations.  Most of the I/O KERNAL
routines call on this routine to create the logical
files to operate on.

No arguments need to be set up to use this routine, but
both the SETLFS and SETNAM KERNAL routines must be
called before using this routine.

### SETNAM - Set file name.

This routine is used to set up the file name for the
OPEN, SAVE, or LOAD routines.  The accumulator must be
loaded with the length of the file name and YX with the
address of the file name.  If no file name is desired
the accumulator must be set to 0 and the YX registers
will be ignored.

SETNAM never returns with an error.

### SETLFS - Set parameters.

SETLFS will set the logical file number (accumulator),
device number (X register), and secondary address (Y
register).

The logical file number is used by the system as a key
to the file table created by the OPEN file routine.
Device addresses can range from 0 to 30.  The following
codes are used by the computer to stand for the
following devices:

~~~
ADDRESS   DEVICE
=======   ======
0        Keyboard
1        Cassette
2        RS-232
3        CRT display
4        Serial bus printer
8        Serial bus disk drive
~~~

Device numbers of four or greater automatically refer
to devices on the serial bus unless they refer to
UltiFS devices emulate being on the serial bus.

A command to the device is sent as a secondary address
on the serial bus after the device number is sent
during the serial attention handshaking sequence.  If
no secondary address is to be sent .Y should be set to
$FF.

SETLFS never returns with an error.

## CLOSE - Close logical file

This routine is used to close a logical file after all
I/O operations have been completed on that file.  This
routine is called after the accumulator is loaded with
the logical file number to be closed, the same number
used when the file was opened using the OPEN routine.


## CHKIN - Open channel for input

Any logical file that has already been opened by the
OPEN routine can be defined as an input channel by this
routine.  The device on the channel must be an input
device or an error will occur and the routine will
abort.

If you are getting data from anywhere other than the
keyboard, this routine must be called before using
either the CHRIN routine or the GETIN routine.  If you
are getting data from the keyboard and no other input
channels are open then the calls to this routine and to
the OPEN routine are not needed.

When used with a device on the serial bus this routine
will automatically send the listen address specified by
the OPEN routine and any secondary address.

Possible errors:

  3 : file not open
  5 : device not present
  6 : file is not an input file

## CHROUT - Open channel for output

Any logical file that has already been opened by the
OPEN routine can be defined as an output channel by
this routine the device on the channel must be an
output device or an error will occur and the routine
will abort.

If you are sending data to anywhere other than the
screen this routine must be called before using the
CHROUT routine.  If you are sending data to the screen
and no other output channels are open then the calls to
this routine and to the OPEN routine are not needed.

When used with a device on the serial bus this routine
will automatically send the listen address specified by
the OPEN routine and any secondary address.

Possible errors:

  3 : file not open
  5 : device not present
  7 : file is not an output file

## CLRCHN - Close input and output channels

This routine is called to clear all open channels and
restore the I/O channels to their original default
values.  It is usually called after opening other I/O
channels and using them for input/output operations.
The default input device is 0, the keyboard.  The
default output device is 3, the screen.

If one of the channels to be closed is to the serial
bus, an UNTALK signal is sent first to clear the input
channel or an UNLISTEN is sent to clear the output
channel.  By not calling this routine and leaving
listener(s) active on the serial bus, several devices
can receive the same data from the VIC at the same
time.  One way to take advantage of this would be to
command the printer to LISTEN and the disk to TALK.
This would allow direct printing of a disk file.

## CHRIN - Input character from channel

This routine will get a byte of data from the channel
already set up as the input channel by the CHKIN
routine.

If CHKIN has not been used to define another input
channel the data is expected to be from the keyboard,
the data byte is returned in the accumulator.  The
channel remains open after the call.

Input from the keyboard is handled in a special way.
First, the cursor is turned on and it will blink until
a carriage return is typed on the keyboard.  All
characters on the logical line, up to 88 characters,
will be stored in the BASIC input buffer.  Then the
characters can be returned one at a time by calling
this routine once for each character.  When the
carriage return is returned the entire line has been
processed and the next time this routine is called the
whole process begins again.

## GETIN - Input character from channel

In practice this routine operates identically to the
CHRIN routine for all devices except for the keyboard.
If the keyboard is the current input device this
routine will get one character from the keyboard
buffer.  It depends on the IRQ routine to ; read the
keyboard and put characters into the buffer.

If the keyboard buffer is empty the value returned in
the accumulator will be 0.

## CHROUT - output a character to channel

This routine will output the byte in the accumulator
to an already opened channel.  Use OPEN and CHKOUT to
set up the output channel before calling this routine.
If these calls are omitted, data will be sent to the
default output device 3, the screen. 

NOTE: Care must be taken when using routine to send
data to a serial device since data will be sent to all
open output channels on the bus.  Unless this is
desired, all open output channels on the serial bus
other than the actually intended destination channel
must be closed by a call to CLOSE before.

## LOAD - load RAM from a device

This routine will load data bytes from any input device
directly into the memory of the computer.  It can also
be used for a verify operation comparing data from a
device with the data already in memory, leaving the
data stored in RAM unchanged.

The accumulator must be set to 0 for a load operation
or 1 for a verify.  If the input device was OPENed with
a secondary address of 0 the header information from
device will be ignored.  In this case .X.Y must contain
the starting address for the load.  If the device was
addressed with a secondary address other than 0 the
data will load into memory starting at the location
specified by the header.  This routine returns the
address after the last byte loaded in YX.

This routine requires SETLFS and SETNAM to be used
before.

## SAVE - save RAM to a device

This routine saves a section of memory.  Memory is
saved from an indirect address on page 0 (specified by
the accumulater) to the address stored in YX to a
logical file.  The SETLFS and SETNAM must be used
before calling this routine.  However, a file name is
not required to SAVE to device 1, the cassette.  Any
attempt to save to other devices without using a file
name results in an error.

NOTE: device 0, the keyboard, and device 3, the screen,
cannot be SAVEd to.  If the attempt is made, an error
will occur, and the SAVE stopped.

## CLACLL - close all channels and files

This routine closes all open files.  When this routine
is called, the pointers into the open file table are
reset, closing all files.  Also the routine
automatically resets the I/O channels.
