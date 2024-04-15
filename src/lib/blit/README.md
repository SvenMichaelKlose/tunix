Software blit library
=====================

Routines designed to copy and move large
blocks of memory rapidly.

# clrram, moveram, memmove

Very compact but fast routines that use
a trick to utilize the zero flag as a
replacement for the carry flag when
counting down.

'memmove' is like 'moveram' but it auto-
detects if a block must be copied
backwards.  That is if the destination
address is lower than the source
address.  It does not (yet) check if
there would be an overlap and backwards
copies come with a performance penalty.
