Code page 437 terminal (40x24 chars)
====================================

# Output

## Supported control codes:

$01,x,y:   Cursor motion
$02:       Insert line
$03:       Delete line
$07:       BEL: flash screen
$08:       BS; Backspace
$09:       HT; Horizontal tab
$0a:       LF: Line feed
$0b:       VF: Vertical tab
$0c:       FF: Form feed, Clear screen
$0d:       CR: Carriage return
$18:       Clear to EOL
$1a:       Clear screen
$1b:       Escape sequences
$1e:       Home
$7f:       DEL: BS, ' ', BS

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
