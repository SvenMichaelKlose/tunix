VIC-20 VI
=========

This a clone of the VI text editor for the Commodore VIC-20
with at least +16K RAM.  It utilizes a 40x24 character
display and undo.

VI keeps a doubly-linked list of lines and uses liblineedit
to edit the 'current' one.  libterm is used for console I/O.
