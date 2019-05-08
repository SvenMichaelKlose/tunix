# G

This is a tiny directory browser for the Commodore VIC-20
with Ultimem expansion.

# Keys

* Cursor up/down: Step up and down in file list.
* Enter: Enter directory or launch program (memory settings auto-detected)
* Cursor left: Back to parent directory.

* N: Step to next window.
* F: Toggle full-screen mode of current window.
* M: Show available memory and largest available block size.

# Missing

* C: Copy
* Space: (Un)select
* A: Select all
* K: Delete
* D: Make directory
* R: Make directory
* C=: Menu

# Memory layout

$0400-$0fff heap
$2000-$3fff switched bank
$4000-$7fff wrappers, shared code, bss, data, stack, heap
$a000-$bfff heap
