# Ingle Flash boot

Loads the desktop and its banks from direcory ".ingle" from
the UltiFS file system on the Flash ROM starting at bank 9.

# API workings

Just the good news right from the start: libingle does all
the necessary stuff to call ROM functions without having to
worry about much, except that libingle should not reside
in BLK5 ($a000-$bfff) as the ROM must be banked in before
any functions of it can be used.

Parameters to functions are being stored at fixed locations.
Just to say it right away: all this isn't too much of a
good idea.

```
$0000:  Start offset of something.
$0004:  Destination offset of something.
$0006:  Word size

$0100-$0103:  Process state signature
$0104-$0105:  misc
$0106:        save stage flag
$0110-$011f:  VIC register set
$0120-$012f:  Ultimem register set
```

First of all that signature does not have to be there.
The ROM could keep track of what's going on in its own
RAM bank.  However, the bottom of the stack page is a
good place to pass parameters in general.  Not using
push and pop instructions is more straightforward and that
quarantees a minimum free stack size.

## Vectors

### $a009 – Save state of current process.

Copies the currently visible RAM to reserved banks.  They are
copied back on reset and restarted at the address in
registers X (high) and A (low).  Also the regular RAM, VIC
and Ultimem registers are copied.

This procedure serves two purposes.  First, to make a copy
of a program to restore it on reset after a crash and second
to save away all unbanked RAM before launching a child process.

Parameters:
```
$0104: restart address
$0106: If not 0, extension banks are also saved.
$0120: Ultimem register set
```

### $a00c – Launch program from ROM

Parameters:
```
$0000 dword offset of program in Ultmem Flash ROM
$0008 word program size
$0104 word start address
$0120 Ultimem register set
```

### $a00f – Allocate RAM bank.

Return bank in register A with carry flag clear.
Returns with carry flag set if out of memory.
Modifies all registers.

### $a012 – Free RAM bank.

Expects bank number in A.  Returns with carry flag set
if bank has not been allocated.  Modifies all registers.

### $a015 – Copy $4000-5fff to $2000

Speed code to copy banks.

### $a018 – Launch program from ROM as new process

To be replacing $a00c perhaps.

Creates a new process with its own RAM banks, not overwriting
the currently running program.  Does only need to copy to save
unbanked RAM which is much faster.

Parameters:
```
* $0000 dword offset of program in Ultmem Flash ROM
* $0008 word program size
* $0104 word start address
* $0120 Ultimem register set
```
