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

# Bank layout

## ROM

```
0: Boot loader + kernal
1-7: at user's disposal, options in boot loader
8-8191: file system
```

## RAM

```
0: desktop RAM1,2,3/IO
1: desktop BLK1
2: desktop BLK2
3: desktop BLK3
4: desktop BLK5
5: desktop desktop UltiFS code
6: desktop 4x8 charset
7: desktop file window code
8-120: free
120-126: saved state
127: Ingle ROM function BSS.
```

# ROM functions

The boot ROM bank provides some utility functions, callable when
mapped to BLK5 ($A000-$BFFF).  libingle provides wrapper functions
that take care of banking.

These functions are being provided by src/flashboot.

# What at least one geek planned

## File manager

### Wanted commands

* C: Copy
* Space: (Un)select
* A: (Un)select all
* D: Delete
* K: Make directory
* R: Rename
* C=: Application menu (if there're additional apps)

## UltiFS hooks/SJLOAD

Enable regular applications to use RAM and ROM as a CBM device.

## Multi-processing

*Not* multi-tasking.
The mechanics of "restore on reset" can also be used to keep
multiple native programs in memory and switch between them.
