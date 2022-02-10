VIC-20 VI
=========

# Commands

Most commands can be prefixed with a
positive integer to repeat them. '10j'
for example will (try to) move the
cursor down ten times.

## Motion

Cursor keys do what you would expect.
h, j, k, l: Left, down, up, right.
0: To first column of line.
$: To last column of line.

## Edit commands

These commands enter the edit mode which
can be exited by pressing ESC.

i: Insert.
I: Insert before first non-space.
o: Open line below.
O: Open line above.
a: Insert past current character.
A: Append to line.
C: Change till end of line.
s: Delete char and insert.

## Modifying commands

These modify the text but do not enter
the edit mode.

D: Delete till line end.
J: Join current and next line.
x: Delete character.

## History commands

.: Repeat last action.

## File commands

:wNAME Write file.
:rNAME Read file.
:kPASSPHRASE Set passphrase for reading/writing files.

# For developers

This is a VI front end for libtext and
liblineedit.  It utilizes a 40x24
character display via libterm.
