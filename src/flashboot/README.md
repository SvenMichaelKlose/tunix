INGLE Flash boot
================

Loads the desktop and its banks from direcory '.ingle' from
the UltiFS file system on the Flash ROM starting at bank 9.


# API

* RAM allocation
* Process switching
* Shared I/O (virtual devices for each process)
* Virtual device for API calls

Parameters to functions are being stored at fixed locations.

```
$0000:  start offset (4 bytes)
$0004:  destination offset (4 bytes)
$0006:  size (2 bytes)

$0100-$0103:  process state signature
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

This procedure serves two purposes.  First, to make a copy
of a program to restore it on reset after a crash and second
to save away all unbanked RAM before launching a child process.

Copies the currently visible RAM to reserved banks.  They are
copied back on reset and restarted at the address in
registers X (high) and A (low).  Also the regular RAM, VIC
and Ultimem registers are copied.

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
the currently running program.  Only needs to save unbanked
RAM which is much faster.

Parameters:
```
* $0000 dword offset of program in Ultmem Flash ROM
* $0008 word program size
* $0104 word start address
* $0120 Ultimem register set
```

# Future

### $a00f – Allocate RAM bank.
### $a012 – Free RAM bank.
### $a015 – Speed copy BLK2 to BLK1.
### $a009 – Save state of current process.
### $a018 – Allocate new process space
### $a018 – Launch program
### $a018 – Copy memory block from another process.
