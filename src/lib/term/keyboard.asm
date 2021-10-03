; Based on matrix table at http://sta.c64.org/cbm64kbdlay.html

.export _get_key

CLR_HOME = 235
BACKSPACE = 8
INS_DEL  = 255
CURSOR_LEFT = 254
CURSOR_RIGHT = 253
CURSOR_UP = 252
CURSOR_DOWN = 251
LEFT_SHIFT = 250
RIGHT_SHIFT = 249
POUND = 248
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

RETURN  = 13

    .data

; TODO: Fix this map and pass it to Simon Rowe.
; 00 1           10 none     20 [SPACE]  30 Q
; 01 3           11 A        21 Z        31 E
; 02 5           12 D        22 C        32 T
; 03 7           13 G        23 B        33 U
; 04 9           14 J        24 M        34 O
; 05 +           15 L        25 .        35 @
; 06 £           16 ;        26 none     36 [ARR UP]
; 07 [DEL]       17 [CSR R]  27 [F1]     37 [F5]
; 08 [ARR LEFT]  18 [STOP]   28 none     38 2
; 09 W           19 none     29 S        39 4
; 0A R           1A X        2A F        3A 6
; 0B Y           1B V        2B H        3B 8
; 0C I           1C N        2C K        3C 0
; 0D P           1D ,        2D :        3D -
; 0E *           1E /        2E =        3E [HOME]
; 0F [RET]       1F [CSR D]  2F [F3]     3F [F7]

devkbd_map_normal:
    .byte "13579+£", BACKSPACE, "wryip*", RETURN
    .byte 0, "adgjl;", CURSOR_RIGHT, RUN_STOP, 0, "xvn,/", CURSOR_DOWN
    .byte " zcbm.", 0, F1, 0, "sfhk:=", F3
    .byte "qetuo@^", F5, "24680-", CLR_HOME, F7
    .byte 0

devkbd_map_shifted:
    .byte "!#%')+£", BACKSPACE, "WRYIP*", RETURN
    .byte 0, "ADGIL]", CURSOR_RIGHT, RUN_STOP, 0, "XVN./", CURSOR_DOWN
    .byte " ZCBM>", 0, F2, 0, "SFHK[=", F4
    .byte "QETUO@^", F6, 34, "$((-", CLR_HOME, F8
    .byte 0

    .code

.proc _get_key
    lda $c6         ; Key in buffer?
    beq _get_key
    lda #0          ; Reset buffer.
    sta $c6
    ldx $c5         ; Get scan code.
    cpx #$40        ; No key pressed?
    beq _get_key
    lda $28d        ; Get CTRL/SHIFT/C= status.
    and #1          ; Shift?
    beq +n
    lda devkbd_map_shifted,x
    rts
n:  lda devkbd_map_normal,x
    rts
.endproc
