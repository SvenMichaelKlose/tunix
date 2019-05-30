# Ingle Flash boot

Loads the desktop and its banks from direcory "ingle" from
the UltiFS file system on the Flash ROM starting at bank 9.

## Vectors

There only one vector at the moment.

### $a009 – Save state of current process.

Copies the currently visible RAM to reserved banks.  They are
copied back on reset and restarted at the address in
registers X (high) and Y (low).  Also the regular RAM, VIC
and Ultimem registers are copied.
