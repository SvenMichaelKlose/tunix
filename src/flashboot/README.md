# Ingle Flash boot

Loads the desktop and its banks from direcory "ingle" from
the UltiFS file system on the Flash ROM starting at bank 9.

## Vectors

There only two entries at the moment.

### $a009 – Save state of current process.

Copies the currently visible RAM to reserved banks.  They are
copied back on reset and restarted at the address in
registers X (high) and A (low).  Also the regular RAM, VIC
and Ultimem registers are copied.

### $a00c – Launch program from ROM

Parameters:

* $0000 dword offset of program in Ultmem Flash ROM
* $0008 word program size
* $0104 word start address
* $0120 Ultimem register set

### $a00f – Allocate RAM bank.

Return bank in register A with carry flag clear.
Returns with carry flag set if out of memory.
Modifies all registers.

### $a012 – Free RAM bank.

Expects bank numer in A.  Returns with carry flag set
if bank has not been allocated.  Modifies all registers.

### $a015 – Copy $4000-5fff to $2000

Speed code to copy banks.

### $a018 – Launch program from ROM as new process

To be replacing $a00c perhaps.

Creates a new process with own RAM banks, not overwriting
the currently running program.  Does only need to save
unbanked RAM which is much faster.

Parameters:

* $0000 dword offset of program in Ultmem Flash ROM
* $0008 word program size
* $0104 word start address
* $0120 Ultimem register set


