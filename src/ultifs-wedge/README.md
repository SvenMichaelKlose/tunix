UltiFS wedge
============

An attempt to hook the C version on top of the KERNAL.

# How it is going to work

The wedge is effectively being loaded to address $2000.
After initialisation (C libs are being used) it copies
itself to other RAM banks to operate from there, leaving
only a small piece of wedge code that will divert KERNAL
calls for the next program on exit.

This "primary wedge" is as small as can be.  It only
banks in the secondary wedge which will finish banking
in the rest of the UltiFS to then finally call the
KERNAL emulating code.  On return, the secondary wedge
brings most of the calling program back in and leaves
the rest to the primary wedge.

This overhead has two advantages.  First, only very few
bytes of the original program space needs to be occupied.
It even fits into unused low memory areas.  Second,
complicated file system code does not have to be written
in assembly.
