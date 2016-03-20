# g – an operating system for the Commodore VIC–20 with Ultimem expansion.

This is a work in progress aiming for

* 32Kb directly addressable RAM for programs
* shared libraries
* multitasking
* hibernating to ROM

without requiring special tools to make applications.

## Memory layout

A program may populate $2000-$7fff and $a000-$bfff.

## System calls


Programs need to request jump tables for system calls or calls to shared
libraries by passing a list ASCIIZ strings to this function.
The first ASCIIZ is the path to the library requested or just "g" to
denote the core in order to keep the components backward–compatible as
long as possible.

Libraries are loaded for each program that request it.

### $0400 – Link process to core or a library.
### "launch" – Launch program on file system.
### "fork" – Create child process.
### "control" – Stop or resume a process.
### "quit" – Quit a process or unload a library.

### "alloc" – Allocate memory bank.
### "free" – Free memory bank.
### "setblock" – Assign bank to block.
### "freeblock" – Free block.

### "create" – Create file.
### "mount" – Mount process to directory.
### "mknode" – Mount process to virtual file.
### "remove" – Remove file or node.
### "open" – Open file.
### "setin" – Set input channel.
### "setout" – Set output channel.
### "chkin" – Check input channel data availability.
### "read" – Read byte.
### "readw" – Read word.
### "readn" – Read multiple bytes.
### "readm" – Read block of unknown size.
### "load" – Like "readm" but also allocating memory banks.
### "write" – Write byte.
### "writew" – Write word.
### "writen" – Write multiple bytes.
### "writem" – Write block starting with size.
### "close" – Close file.

### "cd" – Change working directory.
### "mkdir" – Make directory.
### "rmdir" – Remove directory.

### "settimer" – Set timer.
### "cleartimer" – Clear timer.
### "overtake" – Stop multitasking.
### "release" – Continue multitasking.
