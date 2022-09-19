Terminal emulation
==================

40x24 characters monochrome. Charset
is Code Page 437 (think IBM PC).

# Keyboard

ARROW LEFT:     '~'
ARROW UP:       '^'
SHIFT+-:        '_'
SHIFT+8:        '['
SHIFT+9:        ']'
CTRL+SHIFT+8:   '['
SHIFT+9:        ']'

CTRL-A_:   Control codes 1-31.

# Output

## Control codes:

### $01,x,y:   Cursor motion (CP/M)
### $02:       Insert line (CP/M)
### $03:       Delete line (CP/M)
### $05:       ENQ: Transmit answerback message (vt52, N/A)
### $07:       BEL: flash screen
### $08:       BS; Backspace
### $09:       HT; Horizontal tab
### $0a:       LF: Line feed
### $0b:       VF: Vertical tab (same as line feed)

### $0c:       FF: Form feed (line feed)

Some terminals clear the screen here.

### $0d:       CR: Carriage return
### $0e:       CR: Carriage return

### $18:       Clear to EOL

With vt100 it's CAN (quit control or escape sequence).

### $19:       unused

### $1a:       Clear screen (CP/M)

With vt100 it's CAN (quit control or escape sequence).

### $1b:       Escape sequences
### $1e:       Home
### $7f:       DEL: BS, ' ', BS

## Escape sequences

### Attributes

$0b:     Enable attribute
$0c:     Disable attribute

#### Supported attributes

$01:     Reverse
$10:     Cursor

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
; 09:       HT: Horizontal tabulation
; 1b:       Escape prefix

; Escape:
; 1b:       Quote
; =/Y,x,y:  Cursor motion
; E:        Insert line
; R:        Delete line
; B:        Enable attribute
; C:        Disable attribute
; L:        Set line
; D:        Delete line

; Escape attributes:
; 0         Reverse
; 1         Dark
; 2         Blink
; 3         Underline
; 4         Cursor
; 5         Video
; 6         Cursor position
; 7         Status line

; 01,x,y:   Cursor motion
; 02:       Insert line
; 03:       Delete line
; 07:       BEL: bell and/or flash screen
; 08:       BS; Backspace
; 09:       HT; Horizontal tab
; 0a:       LF: Line feed
; 0b:       VF: Vertical tab (same as line feed)
; 0c:       FF: Form feed, Clear screen
; 1a:       Clear screen
; 0d:       CR: Carriage return
; 1b:       ESC: Escape
; 18:       Clear to EOL
; 1e:       Home
; 7f:       DEL: BS, ' ', BS
