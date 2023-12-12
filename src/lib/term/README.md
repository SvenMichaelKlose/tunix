Terminal emulation
==================

40x24 characters monochrome. Charset is Code Page 437 (think IBM PC).

# Keyboard

ARROW LEFT:     "~"
ARROW UP:       "^"
SHIFT+-:        "_"
SHIFT+8:        "["
SHIFT+9:        "]"
CTRL+SHIFT+8:   "{"
SHIFT+9:        "}"

CTRL-A:   Control codes 1-31.

# Output

## ASCII control codes

These control codes are supported in all modes, except for the
CBM mode.

### $07:       BEL: flash screen
### $08:       BS; Backspace
### $09:       HT; Horizontal tab
### $0a:       LF: Line feed
### $0d:       CR: Carriage return
### $1b:       ESC: Start escape sequence
### $1e:       Home

## CP/M mode control codes

These control code have probably been found in some
CP/M emulator.

### $01,x,y:   Cursor motion (CP/M)
### $02:       Insert line (CP/M)
### $03:       Delete line (CP/M)
### $18:       Clear to EOL
### $1a:       Clear screen (CP/M)
### $7f:       DEL: BS, Space, BS

## Unused control codes

### $05:       ENQ: Transmit answerback message (vt52, N/A)

Same for:
$0b:       VF: Vertical tab (same as line feed)
$0c:       FF: Form feed (line feed)

### $0e:       CR: Carriage return
### $19:       unused

With vt100 it is CAN (quit control or escape sequence).


## Escape sequences

------------------------------
| ESC c     | Reset terminal |
------------------------------

## ANSI escape sequences

----------------------------------------
| ESC col;rowH   | Set cursor position |
----------------------------------------


# Input

```
CLR_HOME = 235
BACKSPACE = 8
INS_DEL  = 255
CURSOR_LEFT = 19
CURSOR_RIGHT = 4
CURSOR_UP = 5
CURSOR_DOWN = 20
LEFT_SHIFT = 250
RIGHT_SHIFT = 249
POUND = 156
ESCAPE = 247
CTRL = 246
COMMODORE = 245
RUN_STOP = 244
F1 = 243
F2 = 242
F3 = 241
F4 = 240
F5 = 239
F6 = 238
F7 = 237
F8 = 236
ARROW_LEFT = 235
RETURN = 13
```
