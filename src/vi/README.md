VIC-20 VI
=========

This is a VI clone with 40x24 char
terminal (code page 437 charset).
It wants a VIC with at least +24K RAM.

# Commands

Most commands can be prefixed with a
positive integer to repeat them. '10j'
for example will (try to) move the
cursor down ten times.

## Motion

Cursor keys do what you would expect:

h, j, k, l: Left, down, up, right.
0: To first column of line.
$: To last column of line.
G: To last line.
w: Move to next char after whitespace.
b: Move to previous word start.

## Editing

These commands enter the edit mode which
can be exited by pressing RUN/STOP.

i: Insert.
I: Insert before first non-space.
o: Open line below.
O: Open line above.
a: Insert past current character.
A: Append to line.
C: Change till end of line.
s: Delete char and insert.

## Modifying lines

These modify the text but do not enter
the edit mode.

D: Delete till line end.
J: Join current and next line.
x: Delete character.
d: Delete line.

## History

.: Repeat last action.

## Files

These commands cannot be repeated.

:wNAME Write file.
:rNAME Read file.
:kPASSPHRASE Set/clear passphrase for
reading/writing files.
