# Ingle

This is a GUI for the Commodore VIC-20 with Ultimem expansion.

# Keys

* Cursor up/down: Step up and down in file list.
* Enter: Enter directory or launch program (memory settings auto-detected)
* Cursor left: Back to parent directory.

* N: Step to next window.
* F: Toggle full-screen mode of current window.
* M: Show available memory and largest available block size.

# Restore on reset

INGLE will jump back to where it was before program launch after reset.
If you press switch1 (the left-most switch on the Ultimem), a regular
boot will be performed.

# Bank layout

## ROM

```
0: Boot loader + kernal
1-7: at user's disposal, options in boot loader
8-8191: file system
```

## RAM

```
0: RAM1,2,3/IO
1: BLK1
2: BLK2
3: BLK3
4: BLK5
5: desktop UltiFS code
6: 4x4 charset
7: desktop file window code
8-11: Program to launch via ROM function.
12-127: allocated on demand
```

# ROM functions

The boot ROM bank will provide some utility functions, callable when
mapped to BLK5 ($A000-$BFFF).  libingle provides wrapper functions
that take care of banking.

## $a009 – Save state

```
void __fastcall__ save_state (unsigned restart_address);
```

Makes a copy of currently mapped RAM contents that will be copied
back on reset. Expects the restart address at $0104/$0105 and the
required Ultimem register set at $0120.

### $a00c – Launch from RAM

```
void __fastcall__ launch (unsigned start, unsigned size);
```

Copies program of $0d/0e bytes from bank 8 on to RAM in $09/$0a
and launches it.


# Roadmap

These are the things planned to have in INGLE.

## File manager

Currently the file manager can navigate through UltiFS and CBM drives
and it can laumch files, auto-detecting required memory configurations
most of the time.

### Wanted commands

* C: Copy
* Space: (Un)select
* A: (Un)select all
* D: Delete
* K: Make directory
* R: Rename
* C=: Application menu (if there're additional apps)

## UltiFS hooks

Enable regular applications to use RAM and ROM as a CBM device.

## Multiprocessing

The mechanics of "restore on reset" can also be used to keep
multiple native programs in memory and switch between them.

## ROM functions

### $a012 – Allocate RAM bank

```
unsigned char alloc_ram_bank (void);
```

### $a015 – Free RAM bank

```
void __fastcall__ free_ram_bank (unsigned char);
```
