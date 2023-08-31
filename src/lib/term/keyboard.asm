.export _term_get

.importzp tmp

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
RETURN  = 13

    .data

; Scan codes
;
; TODO: Pass to Simon Rowe to update his ROM disassembly..
;
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

; SHOFT/C=/CTRL flags at $028d.
FLAG_SHIFT      = 1
FLAG_COMMODORE  = 2
FLAG_CTRL       = 4

map_normal:
map_shift_commodore:
map_shift_ctrl:
map_commodore_ctrl:
map_shift_commodore_ctrl:
    .byte "13579+", POUND, BACKSPACE, ARROW_LEFT, "wryip*", RETURN
    .byte 0, "adgjl;", CURSOR_RIGHT, RUN_STOP, 0, "xvn,/", CURSOR_DOWN
    .byte " zcbm.", 0, F1, 0, "sfhk:=", F3
    .byte "qetuo@^", F5, "24680-", CLR_HOME, F7
    .byte 0

; +, -, POUND, = and RETURN cannot be SHIFTed.
map_shift:
    .byte "!#%')+", POUND, BACKSPACE, ARROW_LEFT, "WRYIP*", RETURN
    .byte 0, "ADGJL]", CURSOR_LEFT, RUN_STOP, 0, "XVN<?", CURSOR_UP
    .byte " ZCBM>", 0, F2, 0, "SFHK[_", F4
    .byte "QETUO@^", F6, 34, "$&(-", CLR_HOME, F8
    .byte 0

map_ctrl:
    .byte "1357", 29, "+", POUND, BACKSPACE, ARROW_LEFT, 23, 18, 25, 9, 16, "*", RETURN
    .byte 0, 1, 4, 7, 10, 12, ";", CURSOR_RIGHT, RUN_STOP, 0, 24, 22, 14, ",", 29, CURSOR_DOWN
    .byte " ", 26, 3, 2, 13, ".", 0, F1, 0, 19, 6, 8, 11, ":=", F3
    .byte 17, 5, 20, 21, 15, 27, 30, F5, "246X0-", CLR_HOME, F7
    .byte 0

map_commodore:
    .byte "13579+", POUND, BACKSPACE, ARROW_LEFT, "wryip*", RETURN
    .byte 0, "ädgjl;", CURSOR_RIGHT, RUN_STOP, 0, "xvn,/", CURSOR_DOWN
    .byte " zcbm.", 0, F1, 0, "sfhk:=", F3
    .byte "qetüö@^", F5, "24680-", CLR_HOME, F7
    .byte 0

maps_l:
    .byte <map_normal, <map_shift, <map_commodore, <map_shift_commodore
    .byte <map_ctrl, <map_shift_ctrl, <map_commodore_ctrl, <map_shift_commodore_ctrl
 
maps_h:
    .byte >map_normal, >map_shift, >map_commodore, >map_shift_commodore
    .byte >map_ctrl, >map_shift_ctrl, >map_commodore_ctrl, >map_shift_commodore_ctrl

    .code

.proc _term_get
    lda $c6         ; Key in buffer?
    beq _term_get
    lda #0          ; Reset buffer.
    sta $c6
    ldy $c5         ; Get scan code.
    cpy #$40        ; No key pressed?
    beq nokey
    ldx $28d        ; Get CTRL/SHIFT/C= status.
    lda maps_l,x    ; Get scan code map.
    sta tmp
    lda maps_h,x
    sta tmp+1
    lda (tmp),y     ; Fetch the code.
    rts

nokey:
    lda #0
    rts
.endproc
