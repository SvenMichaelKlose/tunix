# g – an operating system for the Commodore VIC–20 with Ultimem expansion.

This is a work in progress aiming for

* 34Kb directly addressable RAM for programs
* shared libraries
* multitasking
* hibernating to ROM

without requiring special tools to make applications.

A compiled version can be downloaded from http://hugbox.org/pixel/external/denial/g.zip

## Memory layout

A program may populate $2000-$7fff and $a000-$bfff.

## Program format

g automatically loads "sh" which you can replace by your own application
to fool around.

```
load address (2 bytes)
program size (2 bytes)
program
```

The load address does NOT include the address and size.

## System calls

Programs need to request jump tables for system calls or calls to shared
libraries by passing a list ASCIIZ strings to the function at $0400.
The first ASCIIZ string is the path to the library requested in order to
keep the components backward–compatible as long as possible.

Libraries are loaded for each program that request it.

### Core zero page locations

#### $0000: s – Source pointer
#### $0002: d – Destination pointer
#### $0004: c – Counter/extra pointer
#### $0006–$000f: Reserved

### Process management
#### $0400 – Link process to core or a library.
```
s: ASCIIZ library path followed zero–terminated list of ASCIIZ system call names.
d: Jump table address.
```

To link core functions (the only thing that can be linked at this time),
specify "/g".

#### "launch" – Launch program on file system.
```
s: Program name (ASCIIZ string).

Returns:
A: Process ID.
```

#### "fork" – Create child process.
#### "control" – Stop or resume a process.
#### "quit" – Quit a process or unload a library.

### Strings
#### "inc_s" – Increment pointer s.
#### "inc_d" – Increment pointer d.
#### "compare_asciiz" – Compare ASCIIZ strings at s and d.

### Memory
#### "alloc" – Allocate memory bank.
#### "free" – Free memory bank.
#### "setblock" – Assign bank to block.
#### "freeblock" – Free block.

### File I/O
#### "create" – Create file.
#### "mount" – Mount process to directory.
#### "mknode" – Mount process to virtual file.
#### "remove" – Remove file or node.
#### "open" – Open file.
#### "setin" – Set input channel.
#### "setout" – Set output channel.
#### "chkin" – Check input channel data availability.
#### "read" – Read byte.
#### "readw" – Read word.
#### "readn" – Read multiple bytes.
#### "readm" – Read block of unknown size.
#### "load" – Like "readm" but also allocating memory banks.
#### "write" – Write byte.
#### "writew" – Write word.
#### "writen" – Write multiple bytes.
#### "writem" – Write block starting with size.
#### "close" – Close file.

### Directories
#### "cd" – Change working directory.
#### "mkdir" – Make directory.
#### "rmdir" – Remove directory.

### Timer
#### "settimer" – Set timer.
#### "cleartimer" – Clear timer.

### Multitasking control
#### "take_over" – Stop multitasking.
#### "release" – Continue multitasking.
