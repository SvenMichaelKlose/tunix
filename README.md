# Ingle

This is a GUI for the Commodore VIC-20 with Ultimem expansion.

# Keys

* Cursor up/down: Step up and down in file list.
* Enter: Enter directory or launch program (memory settings auto-detected)
* Cursor left: Back to parent directory.

* N: Step to next window.
* F: Toggle full-screen mode of current window.
* M: Show available memory and largest available block size.

# Roadmap

## Wanted commands

* C: Copy
* Space: (Un)select
* A: Select all
* D: Delete
* K: Make directory
* R: Rename
* C=: Menu

## Hook up KERNAL for regular file access to UltiFS

Enable regular applications to use RAM and ROM as a CBM device.

## Stack of processes

Save program state to Ultimem RAM, launch app, restore on reset.
