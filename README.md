# Ingle

This is a GUI for the Commodore VIC-20 with Ultimem expansion.

# File manager

Currently the file manager (or "desktop") can navigate through
UltiFS and CBM drives and it can laumch files, auto-detecting
required memory configurations most of the time.

## Restore on reset

The desktop will jump back to where it was before program
launch after reset.  If you press switch1 (the left-most switch
on the Ultimem), a regular boot will be performed.

## Keys

* Cursor up/down: Step up and down in file list.
* Enter: Enter directory or launch program (memory settings auto-detected)
* Cursor left: Back to parent directory.

* N: Step to next window.
* F: Toggle full-screen mode of current window.
* M: Show available memory and largest available block size.
