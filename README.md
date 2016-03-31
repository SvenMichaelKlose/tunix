# g – an operating system for the Commodore VIC–20 with Ultimem expansion.

This is a work in progress aiming for

* 28Kb directly addressable RAM for programs
* shared libraries
* multitasking
* hibernating to ROM

without requiring special tools to make applications.

A compiled version can be downloaded from
http://hugbox.org/pixel/external/denial/g.zip
A version of the cc65 C compiler with "vic20g" target is to be found at
https://github.com/SvenMichaelKlose/cc65g

## Memory layout

A program may populate $2000-$7fff and use the IO area but it must not
hide the Ultimem expansion's registers.

## Program format

g automatically loads "sh" which you can replace by your own application
to fool around.

```
load address (2 bytes)
program size (2 bytes)
program code
```

The load address does NOT include the address and size.

## Library format

Libraries are programs like described above with an additional header
containing the names of available calls as ASCIIZ strings and their
addresses. Here's an example of a short symbol index:

```
    .word index_size    ; Does NOT include these two bytes.
    .asciiz "call1"
    .word call1
    .asciiz "call2"
    .word call2
    .byte 0             ; End of index.
```

All in all libraries have the following format:

```
symbol index size (2 bytes)
symbol index
load address (2 bytes)
library size (2 bytes)
library code
```

## System calls

Programs need to request jump tables via the function at address $0400.

### Errors

Core functions return with the carry flag set if an error occurred.

### Core zero page locations

#### $0000: s – Source pointer
#### $0002: d – Destination pointer
#### $0004: c – Counter/extra pointer
#### $0006–$000f: Reserved

### Process management
#### $0400 – Link process to core or a library.
```
s: ASCIIZ library path followed zero–terminated list of ASCIIZ system call names.  Pass "/g" to link to core functions.
d: Jump table address.
```

Example to access the core:
```
;; Say hello, launch another program and exit.

program_size = program_end - program_start

    .word program_start  ; Load address (not loaded).
    .word program_size   ; Does not affect the load address.

    org $2000

program_start:
    ;; Request core system call jump table.
    lda #<symbols
    sta s
    lda #>symbols
    sta s+1
    lda #<jump_table
    sta d
    lda #>jump_table
    sta d+1
    jsr $0400
    bcs error

    ;; Say something nice.
    ldx #<txt_welcome
    ldy #>txt_welcome
    jsr print

    ;; Launch program.
    lda #<path_program
    sta s
    lda #>path_program
    sta s+1
    jsr launch

    rts

;; Print a zero–terminated string.
print:                                                                          
    stx s
    sty @(++ s)
l:  ldy #0
    lda (s),y
    beq +done
    jsr take_over   ; Stop multitasking.
    jsr $ffd2       ; Print character via KERNAL.
    jsr release     ; Resume multitasking.
    jsr inc_s
    jmp l
done:
error:
    rts

txt_welcome:    .asciiz "HELLO WORLD!"
path_program:   .asciiz "MYPROG"

symbols:
    .asciiz "/g"        ; Function in the core please.
    .asciiz "take_over"
    .asciiz "release"
    .asciiz "inc_s"
    .asciiz "launch"
    .byte 0             ; End of symbol list.

jump_table:
take_over: .byte 0, 0, 0
release:   .byte 0, 0, 0
inc_s:     .byte 0, 0, 0
launch:    .byte 0, 0, 0

program_end:
```

#### "launch" – Launch program on file system.
```
A: If not 0 the parent process is halted until the launched process is killed.
s: Program name (ASCIIZ string).

Returns:
A: Process ID.
```

Loads a program and runs it independently of the invoking task.

#### "fork" – Create child process.

#### "halt" – Halt a process.
```
A: Process ID.
```

Can be resumed with "resume".

#### "resume" – Resumes a halted process.
```
A: Process ID.
```

#### "kill" – Kill a process and its libraries
```
A: Process ID.
```

Does not return if process killed itself.

### Strings
#### "inc_s" – Increment pointer s.

Increments zero page pointer "s".

#### "inc_d" – Increment pointer d.

Increments zero page pointer "d".

#### "compare_asciiz" – Compare ASCIIZ strings at s and d.
```
s:  String A.
d:  String A.

Returns:
Zero flag clear if strings match.
```

### Memory
#### "alloc_bank" – Allocate memory bank.
```
Returns:
tmp: Allocated bank number.
```

#### "free_bank" – Free memory bank.
```
A: Bank number
```

#### "moveram" – Move memory block.
```
A:  0 = forwards, 1 = backwards
s: Source address
d: Destination address
c: Number of bytes
```

#### "clrram" – Clear memory block.
```
d: Destination address
c: Number of bytes
```

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

Turns off multitasking and restores the NMI vector.  Use this before doing
time–critical operations or calling KERNAL functions.

Calls to "take_over" may happen multiple times.

#### "release" – Continue multitasking.

Turns on multitasking and diverts the NMI vector to the task switcher.
Flags and registers are not affected.

"release" has to be called as often as "take_over" before multitasking is
actually turned back on.

## Virtual file system

To support multitasking, g maintains a set of file descriptors for each
process.

### vfile structure

Each file descriptor points to a vfile that is global and unique. When
many processes access the same, file their file handles point
to the same vfile. vfiles are created when a file is opened and removed
when it isn't used by a process anymore. Only the vfile for the root
directory is never removed.
