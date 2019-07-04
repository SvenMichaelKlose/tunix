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

### $a00c – Launch program

The program size is expected at $0008/$0009, the start
address at $0124/$0125 and the Ultimem register set at
$0120-$012f.  The program is copied from bank 12 on in to
the RAM as configured by that register set.

### $a00f – Allocate RAM bank.

Return bank in register A with carry flag clear.
Returns with carry flag set if out of memory.
Modifies all registers.

### $a012 – Free RAM bank.

Expects bank numer in A.  Returns with carry flag set
if bank has not been allocated.  Modifies all registers.
