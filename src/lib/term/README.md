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

$0a:    ENTER
