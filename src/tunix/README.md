TUNIX
=====

Multi-tasking KERNAL I/O extension for
the Commodore VIC-20 with UltiMem
expansion.

Defective UltiMem RAM banks are detected
and disabled when TUNIX starts.

# Status: under construction

Slowly growing the TDD way.

# Wanted features

* Highly-portable UNIX-style KERNAL
  I/O API.
* Pre-emptive multi-tasking.
* +35K RAM for all processes.
* Loadable drivers.

# System calls

System calls are performed via calls
to OPEN on device #31.  The first letter
of the file name denotes the command
group, the second the command in that
group.  Argument bytes may follow.

Downcase letters in the examples are
placeholders for argument bytes.

After sending a system call via OPEN
an error code may be read using BASIN.
(Calling CHKIN is not required.)  It may
followed by return values.

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

## General

### "": Schedule

Gives TUNIX the opportunity to switch to
the next process without doing anything
else.  Like the other system calls it
may switch unless in single-tasking mode
(command "SS").

### "SM": Multi-task

Enables task-switching via interrupt.
The default.  Task switches are
scheduled after all system calls.

### "SS": Single-task

Disables task-switching.  Should be used
sparingly.

### "SX": Shut down

Closes all files, kills all processes
and returns to plain BASIC.

## Processes

### "PL": Process list

Returns a PASCAL string (starting with
a length byte) of process flags.

### "PI": Process ID

Returns current process ID byte.

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

### "PEname and args": Execute

Replaces the current process by an
executable in TUNIX format.  But fear
not:  'runprg' and 'basic' will also
launch native programs.

### "PXc": Exit

Exits the current process with an exit
code.  It remains on the process list
until its parent exits or returns from a
wait() on it.  Child processes keep
running with the init process (0) as
their parent.

### "PWp": Wait

Waits for a process to exit and returns
its exit code.

### "PKp": Kill

Exits any process with exit code 255.

### "PSp": Stop

Moves process to the sleeping list.

### "PRp": Resume

Moves process to the running list.

### "PTp": Terminate

For drivers to exit and move to the
sleeping list instead of getting killed.

## Extended memory

## "MA": Allocate a bank

## "MFb": Free a bank

## Drivers

Processes may register as a driver for
a particular device with its IO vector
table.  TUNIX translates logical file
names to global ones that may be shared
by processes.  An LFN is always trans-
lated to the same GLFN until the process
exits.

Assign GLFN to device 0+3.

### "DL": Driver list

### "DRdvv": Register

Registers a driver for a device.  Global
to all processes.  It unregisters when
the driver exits.

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
