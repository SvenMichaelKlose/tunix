INGLE – The cozy GUI for your Commodore VIC-20 with UltiMem expansion
=====================================================================

So far just a public coding experiment.

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
* D: Hide windows.

# What's planned

INGLE is intended to provide a multi-processing GUI to all VIC programmers
no matter what language by adding a RAM and ROM disk and virtual devices
to the CBM KERNAL.

# Programming for the desktop

The desktop can be controlled via device #31.

## Opening a window

```basic
OPEN 31, "C" + CHR(30) + ":Window title", 31
GET# 31, ERROR
CLOSE 31
IF ERROR THEN SYS 49872
OPEN 30, "", 30
PRINT# 30, "
```

