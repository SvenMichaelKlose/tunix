.export _keyboard_get
.importzp tmp

.include "libkeyboard.inc"

VIA2PB      = $9120 ; VIA 2 DRB, keyboard column drive
VIA2PA1     = $9121 ; VIA 2 DRA, keyboard row port

; VIC 20 keyboard matrix layout
;       c7     c6   c5   c4    c3    c2    c1    c0
;   +----------------------------------------------------------------
; r7|   [F7]   [F5] [F3] [F1]  [DWN] [RGT] [RET] [DEL]
; r6|   [HOME] [UP] =    [RSH] /     ;     *     £
; r5|   -      @    :    .     ,     L     P     +
; r4|   0      O    K    M     N     J     I     9
; r3|   8      U    H    B     V     G     Y     7
; r2|   6      T    F    C     X     D     R     5
; r1|   4      E    S    Z     [LSH] A     W     3
; r0|   2      Q    [C=] [SP]  [RUN] [CTL] [<-]  1

BACKSPACE       = 8
CURSOR_LEFT     = 19
CURSOR_RIGHT    = 4
CURSOR_UP       = 5
CURSOR_DOWN     = 20
POUND           = 156
CLR_HOME        = 248
ESCAPE          = 247
RUN_STOP        = 244
F1              = 243
F2              = 242
F3              = 241
F4              = 240
F5              = 239
F6              = 238
F7              = 237
F8              = 236
ARROW_LEFT      = 235
RETURN          = 13

    .data

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

    .bss

num_scancodes:          .res 1
pressed_keys:           .res 64
formerly_pressed_keys:  .res 64

    .code

.proc _scan
    ldx #$3f
    ldy #0
l3: lda pressed_keys,x
    sta formerly_pressed_keys,x
    tya
    sta pressed_keys,x
    dex
    bpl l3

    lda #0
    sta num_scancodes
    sta VIA2PB
    ldx VIA2PA1
    inx
    beq no_key

    ldx #8
    lda #$fe
l2: sta VIA2PB
l:  lda VIA2PA1
    lsr
    bcs n
    lda pressed_keys,y
    bne n           ; Already pressed…
    lda #1
    sta pressed_keys,y
    inc num_scancodes
n:  iny
    dex
    bpl l
    sec
    rol VIA2PB
    bne l2          ; (jmp)
no_key:
    rts
.endproc
    
.proc _keyboard_get
    lda $c6         ; Key in buffer?
    beq _keyboard_get
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
