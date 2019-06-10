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

## Restore on reset

The file manager is supposed to save its state before launching another
program which the boot loader should restore instantly on reset.

## Multiprocessing

The mechanics of "restore on reset" can also be used to keep
multiple native programs in memory and switch between them.

## ROM functions

The boot ROM bank will provide some utility functions, callable when
mapped to BLK5 ($A000-$BFFF).

### $A009 – Push state

Makes a copy of currently mapped RAM contents that will be copied
back on reset. Expects the restart address in register A (low) and
X (high).

Multiple calls of this will stack up the states.

### $A009 – Pop state

Pops the last saved state from the stack.

### $A00C – Launch from RAM

```
void __fastcall__ launch (unsigned start, unsigned size);
```

### $A00F – Allocate RAM bank

```
unsigned char alloc_ram_bank (void);
```

### $A012 – Free RAM bank

```
void __fastcall__ free_ram_bank (unsigned char);
```
