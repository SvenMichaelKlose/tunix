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
    INS_DEL RETURN CURSOR_RIGHT F7 F1 F3 F5 CURSOR_DOWN
    "3wa4zse" LEFT_SHIFT
    "5rd6cftx"
    "7yg8bhuv"
    "9ij0mkon"
    "+pl-.:@,"
    POUND "*;" CLR_HOME RIGHT_SHIFT "=^/"
    "1" ESCAPE CTRL 2 " " COMMODORE "q" RUN_STOP

devkbd_map_normal:
    INS_DEL RETURN CURSOR_LEFT F8 F2 F4 F6 CURSOR_UP
    "#WA$ZSE" LEFT_SHIFT
    "%RD&CFTX"
    "'YG(BHUV"
    ")IJ0MKON"
    "+PL->[@<"
    POUND "*]" CLR_HOME RIGHT_SHIFT "=^?"
    "!" ESCAPE CTRL "\"" " " COMMODORE "Q" RUN_STOP

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
    sei
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
    cli
    rts

wait_key:
    ldx #0
    ldy #0
    jsr scan_keyboard
    bcs wait_key

    stx tmp
    tya
    asl
    asl
    asl
    ora tmp
    tay
    lda devkbd_map_normal,y
    rts

reverse_bits:
    %01111111
    %10111111
    %11011111
    %11101111
    %11110111
    %11111011
    %11111101
    %11111110
