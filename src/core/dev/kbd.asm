; Based on matrix table at http://sta.c64.org/cbm64kbdlay.html

CLR_HOME = 235
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

devkbd_map_normal:
    "1" RETURN CURSOR_RIGHT CURSOR_DOWN " " F3 "q" "2"
    "3wa" LEFT_SHIFT "zse4"
    "5rdxcft6"
    "7ygvbhu8"
    "9ijnmko0"
    "+pl,.:@-"
    POUND "*;/" RIGHT_SHIFT "=^" CLR_HOME
    INS_DEL ESCAPE CTRL RUN_STOP F1 COMMODORE F5 F7

devkbd_map_shifted:
    "!" RETURN CURSOR_LEFT CURSOR_UP " " F4 "Q" "\""
    "#WA" LEFT_SHIFT "ZSE$"
    "%RDXCFT&"
    "'YGVBHU("
    ")IJNMKO0"
    "+PL<>[@-"
    POUND "*]?" RIGHT_SHIFT "=^" CLR_HOME
    INS_DEL ESCAPE CTRL RUN_STOP F2 COMMODORE F6 F8

; Based on http://vicpp.blogspot.de/2012_06_01_archive.html

via2_portb0 = $9120
via2_porta0 = $9121

; X: row mask
; Y: column mask
;
; Returns:
; X: row
; Y: column
scan_keyboard:
    txa
    eor #$ff
    sta column_mask
    sty row_mask

    ldy #7
next_column:
    ldx #7
    lda bits,y
    and column_mask
    beq no_keypress
    eor #$ff
    sta via2_portb0

    lda via2_porta0
    ora row_mask

next_row:
    asl
    bcc got_row

    dex
    bpl next_row

    dey
    bpl next_column

no_keypress:
    sec
got_row:
    rts

wait_key:
    ldx #0
    ldy #0
    jsr scan_keyboard
    bcc wait_key
l:  ldx #0
    ldy #0
    jsr scan_keyboard
    bcs -l

    sty tmp
    txa
    asl
    asl
    asl
    ora tmp
    tay
    lda devkbd_map_normal,y
    rts
