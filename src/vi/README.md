VIC-20 VI
=========

# Commands

## Motion

Cursor keys do what you would expect.
h, j, k, l: Left, down, up, right.
0: To first column of line.
$: To last column of line.

## Edit commands

These commands enter the edit mode which
can be exited by pressing ESC.
They cannot be repeated automatically by
prefixing them with numbers.

i: Insert.
I: Insert before first non-space.
o: Open line below.
O: Open line above.
a: Insert past current character.
A: Append to line.
s: Delete char and insert.

## Modifying commands

These modify the text but do not enter
the edit mode.

D: Delete till line end.
J: Join current and next line.
x: Delete character.

# For developers

This is a VI front end for libtext and
liblineedit.  It utilizes a 40x24
character display via libterm.
