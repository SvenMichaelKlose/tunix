; Input control codes:
; 03:   Page down
; 04:   Arrow right
; 05:   Arrow up
; 07:   Delete
; 17:   Home/End
; 18:   Page up
; 19:   Arrow left
; 20:   Arrow down
; 22:   Insert

; Output control codes:
; 01,x,y:   Cursor motion
; 02:       Insert line
; 03:       Delete line
; 07:       BEL: beep and/or flash screen
; 08:       BS; Backspace
; 09:       HT: Horizontal tabulation
; 0a:       LF: Line feed
; 0c:       FF: Form feed, Clear screen
; 0d:       CR: Carriage return
; 18:       Clear to EOL
; 1a:       Clear screen
; 1b:       Escape prefix
; 1e:       Home
; 7f:       DEL: BS, ' ', BS

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
