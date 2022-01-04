UltiFS KERNAL extension
=======================

Start with BLK1, BLK2, BLK3 and IO1,2 enabled.

# Anatomy

UltiFS has been written in C mostly.
On launch it copies itself to RAM banks 117 (BLK1) to 119
(BLK3), modifies the KERNALs I/O vectors to point at a
wedge at $9800 and does a BASIC cold start.  The primary
wedge is responsible for banking the secondary wedge in
to BLK1.  It checks if the device number is #12.  If it's
not, it returns to the primary wedge to bank the old BLK1
back in and to continue with whatever procedure the former
KERNAL vector pointed to.  Otherwise BLK2 and BLK3 of the
UltiFS is banked in, the zeropage is saved and the UltiFS
procedure in question (OPEN, CLOSE, BSIN, etc.) is
invoked.  When it's finished, the saved (and probably
modified zeropage) is copied back over the original, BLK1
to BLK3 are being restored and the registers and flags
are set before returning to the caller.
