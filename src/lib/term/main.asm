.include "cbm_kernal.inc"

.export _term_init, _term_put, _term_puts, _term_putsn
.export term_init, term_puts

.import init_bitmap_mode, gfx_init
.import clear_screen
.import box
.import putchar_fixed
.import _pattern_empty
.import _pattern_solid
.import init_region_stack
.import reset_region
.import moveram
.import calcscr
.import charset, charset_size
.import popax

.import box

.importzp s, d, c, scrbase, scr
.importzp font, pencil_mode, pattern
.importzp xpos, ypos, xpos2, ypos2, width, height
.importzp screen_width, screen_height, screen_columns, screen_rows

ATTR_BOLD       = 1
ATTR_HALFBRIGHT = 2
ATTR_UNDERLINE  = 4
ATTR_BLINK      = 8
ATTR_REVERSE    = 16
ATTR_CURSOR     = 32

    .zeropage

tmp:            .res 2
tmp2:           .res 1
p:              .res 2

    .bss

cursor_x:       .res 1
cursor_y:       .res 1
has_cursor:     .res 1
visible_cursor: .res 1
code:           .res 3
code_length:    .res 1
code_callback:  .res 2
attributes:     .res 2
last_in:        .res 1

    .data

our_charset:
    .include "charset-4x8.asm"

code_handlers:
    .word do_nothing
    .word init_cursor_motion   ; 01,x,y:   Cursor motion
    .word insert_line          ; 02:       Insert line
    .word delete_line          ; 03:       Delete line
    .word do_nothing           ; 04
    .word do_nothing           ; 05
    .word do_nothing           ; 06
    .word bell                 ; 07:       BEL: bell and/or flash screen
    .word cursor_left          ; 08:       BS; Backspace
    .word tab                  ; 09:       HT; Horizontal tab
    .word cursor_down          ; 0a:       LF: Line feed
    .word cursor_down          ; 0b:       VF: Vertical tab
    .word form_feed            ; 0c:       FF: Form feed,
    .word carriage_return      ; 0d:       CR: Carriage return
    .word do_nothing           ; 0e
    .word do_nothing           ; 0f
    .word do_nothing           ; 10
    .word do_nothing           ; 11
    .word do_nothing           ; 12
    .word do_nothing           ; 13
    .word do_nothing           ; 14
    .word do_nothing           ; 15
    .word do_nothing           ; 16
    .word do_nothing           ; 17
    .word clear_to_eol         ; 18:       Clear to EOL
    .word do_nothing           ; 19
    .word form_feed            ; 1a:       Clear screen
    .word escape               ; 1b:       ESC: Escape
    .word do_nothing           ; 1c
    .word do_nothing           ; 1d
    .word home                 ; 1e:       Home

    .code

.proc _term_init
    lda #1
    ldx #0
    ldy #2
.endproc

.proc term_init
    jsr init_bitmap_mode
    lda #$80
    sta $291    ; Block character set switch with Shift+C=.
    jsr clear_screen
    lda #$00
    sta scrbase
    lda #$11
    sta scrbase+1

    jsr init_region_stack
    jsr reset_region

    lda #<our_charset
    sta font
    sta p
    lda #>our_charset
    sta font+1
    sta p+1

    ; Double each chars half.
    ldy #0
l2: lda (p),y
    sta tmp
    asl
    asl
    asl
    asl
    ora tmp
    sta (p),y
    iny
    bne l2
    inc p+1
    lda p+1
    cmp #>our_charset + 8
    bne l2
.endproc

.proc term_reset
    ; Init terminal state.
    ldy #0
    sty cursor_x
    sty cursor_y
    sty visible_cursor
    sty code_callback+1
    iny
    sty has_cursor
    ldy #$ff
    sty code_length
    lda #ATTR_CURSOR
    sta attributes
    jmp cursor_draw
.endproc

.proc scroll_up
    ; Clear top row (except first char).
    lda #8
    sta xpos
    sta height
    lda #0
    sta ypos
    lda #screen_width-8
    sta width
    lda #1
    sta pencil_mode
    lda #<_pattern_empty
    sta pattern
    lda #>_pattern_empty
    sta pattern+1
    jsr box

    ; Move all of the screen by one char.
    ; Cleared top row is now at the bottom.
    lda #<(charset + 8)
    sta s
    lda #<charset
    sta d
    lda #>charset
    sta s+1
    sta d+1
    lda #<(charset_size - 8)
    sta c
    lda #>(charset_size - 8)
    sta c+1
    lda #0
    jsr moveram

    ; Clear char in the bottom right corner.
    ldx #7
    lda #0
l2: sta charset + charset_size - 8,x
    dex
    bpl l2

    rts
.endproc

.proc scroll_down
    rts
.endproc

.proc cursor_draw
    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos

    lda attributes
    and #ATTR_CURSOR
    beq r

    lda xpos
    cmp #screen_width
    bcc n
    lda ypos
    clc
    adc #8
    sta ypos
    lda #0
    sta xpos
n:
    lda ypos
    cmp #screen_height
    bcs r

    lda #<_pattern_solid
    sta pattern
    lda #>_pattern_solid
    sta pattern+1
    lda #2
    sta pencil_mode
    asl
    tax
    stx width
    asl
    sta height
    jmp box

r:  rts
.endproc

.proc cursor_hide
    pha
    txa
    pha
    tya
    pha
    lda visible_cursor
    inc visible_cursor
    ora #0
    bne r
    jsr cursor_draw
r:  pla
    tay
    pla
    tax
    pla
    rts
.endproc

.proc cursor_show
    pha
    txa
    pha
    tya
    pha
    dec visible_cursor
    bne r
    jsr cursor_draw
r:  pla
    tay
    pla
    tax
    pla
    rts
.endproc

.proc cursor_step
    lda cursor_x
    clc
    adc #4
    sta cursor_x
    cmp #160
    bne n
    lda #0
    sta cursor_x
    jmp cursor_down
n:  rts
.endproc

.proc cursor_left
    lda cursor_x
    beq r
    lda cursor_x
    sec
    sbc #4
    sta cursor_x
r:  rts
.endproc

.proc cursor_down
    lda cursor_y
    cmp #screen_height - 8
    bne n
    jmp scroll_up
n:  clc
    adc #8
    sta cursor_y
    rts
.endproc

.proc cursor_up
    lda cursor_y
    bne n
    jmp scroll_down
n:  sec
    sbc #8
    sta cursor_y
    rts
.endproc

.proc carriage_return
    lda #0
    sta cursor_x
    rts
.endproc

.proc exec_cursor_motion
    tay
    dey
    cpy #screen_rows * 2
    bcs r
    ldx code
    dex
    cpx #screen_columns
    bcs r
    jsr cursor_hide
    txa
    asl
    asl
    sta cursor_x
    tya
    asl
    asl
    asl
    sta cursor_y
    jsr cursor_show
r:  lda #0
    sta code_callback+1
    rts
.endproc

.proc read_cursor_motion
    sta code
    lda #<exec_cursor_motion
    sta code_callback
    lda #>exec_cursor_motion
    sta code_callback+1
    rts
.endproc

.proc init_cursor_motion
    lda #<read_cursor_motion
    sta code_callback
    lda #>read_cursor_motion
    sta code_callback+1
    rts
.endproc

.proc bell
    ldy #$0c
    ldx #0
l:  lda $900f
    pha
    and #$f8
    sta tmp
    pla
    clc
    adc #1
    and #$07
    ora tmp
    sta $900f
    dex
    bne l
    dey
    bne l
    lda #$0a
    sta $900f
    rts
.endproc

.proc clear_to_eol
    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos
    lda #<_pattern_empty
    sta pattern
    lda #>_pattern_empty
    sta pattern+1
    lda #1
    sta pencil_mode
    lda #160
    sec
    sbc cursor_x
    sta width
    lda #8
    sta height
    jmp box
.endproc

; 09:       HT: Horizontal tabulation
.proc tab
    lda cursor_x
    and #%11100000
    clc
    adc #%00100000
    sta cursor_x
    cmp #160
    bne done
    sec
    sbc #160
    sta cursor_x
done:
    rts
.endproc

.proc insert_line
    ; Move to line start.
    lda #0
    sta cursor_x
    sta xpos
    lda cursor_y
    cmp #screen_height-8
    beq done
    sta ypos

    ; Move down columns.
    lda #screen_height-8-1
    sec
    sbc ypos
    sta height  ; -1 for Y indexing.
    lda #20
    sta width
l2: jsr calcscr
    lda scr
    clc
    adc #8
    sta tmp
    lda scr+1
    adc #0
    sta tmp+1
    ldy height
    ldx height
    inx
l:  lda (scr),y
    sta (tmp),y
    dey
    dex
    bne l

    lda xpos
    clc
    adc #8
    sta xpos
    dec width
    bne l2

done:
    jmp clear_to_eol
.endproc

.proc delete_line
    ; Move to start of line.
    lda #0
    sta cursor_x

    lda cursor_y
    cmp #screen_height-8
    beq done

    ; Move up lines below.
    sta ypos
    lda #screen_height-8-1
    sec
    sbc ypos
    sta height
    lda #0
    sta xpos
    lda #20
    sta width
l2: jsr calcscr
    lda scr
    clc
    adc #8
    sta tmp
    lda scr+1
    adc #0
    sta tmp+1

    ldy #0
    ldx height
    inx
l:  lda (tmp),y
    sta (scr),y
    iny
    dex
    bne l

    lda xpos
    clc
    adc #8
    sta xpos

    dec width
    bne l2
    
done:
    ; Clear last line.
    lda cursor_y
    pha
    lda #screen_height-8
    sta cursor_y
    jsr clear_to_eol
    pla
    sta cursor_y
    rts
.endproc

.proc form_feed
    jsr clear_screen
    jmp home
.endproc

.proc home
    lda #0
    sta cursor_x
    sta cursor_y
    rts
.endproc

; 7f:       DEL: BS, ' ', BS
.proc delete
    lda #7
    jsr _term_put
    lda #32
    jsr _term_put
    lda #7
    jsr _term_put
    jmp cursor_show
.endproc

.proc ansi_home
    ldx code
    dex
    cpx #screen_columns
    bcs r
    ldy code+1
    dey
    cpy #screen_rows
    bcs r
    tya
    asl
    asl
    asl
    sta cursor_y
    txa
    asl
    asl
    sta cursor_x
r:  rts
.endproc

.proc aa_reset
    lda #0
    sta attributes
    rts
.endproc

.proc ansi_attr
    ldx code
    beq aa_reset
    cpx #8
    bcs n
    lda ansi_attr_bits,x
    ora attributes
    sta attributes
r:  rts

n:  cpx #27
    bcc r
    txa
    sec
    sbc #21
    bcc r
    tax
    lda ansi_attr_bits,x
    eor #$ff
    and attributes
    sta attributes
    rts
.endproc

ansi_attr_bits:
    .byte 0, ATTR_BOLD, ATTR_HALFBRIGHT, 0, ATTR_UNDERLINE, ATTR_BLINK, 0, ATTR_REVERSE

ansi_codes:
    .byte "H"
    .byte "m"
    .byte 0
ansi_codes_hl:
    .byte <ansi_home, <ansi_attr
ansi_codes_hh:
    .byte >ansi_home, >ansi_attr

.proc exec_ansi
    ldx code_length
    cpx #3
    beq invalid
    cmp #';'
    bne n
    inc code_length
    bne r   ; (jmp)
    
invalid:
    jsr bell
reset_callback:
    lda #0
    sta code_callback+1
    rts

n:  cmp #'0'
    bcc call
    cmp #'9'+1
    bcs call
    ldx code_length
    lda code,x
    asl
    sta tmp
    asl
    asl
    adc tmp
    adc last_in
    sec
    sbc #'0'
    sta code,x
r:  rts

call:
    ldx #0
l:  lda ansi_codes,x
    beq invalid
    cmp last_in
    beq found
    inx
    jmp l

found:
    jsr reset_callback
    lda ansi_codes_hl,x
    sta tmp
    lda ansi_codes_hh,x
    sta tmp+1
    jmp (tmp)
.endproc

.proc ansi_escape
    lda #0
    sta code_length
    sta code
    sta code+1
    sta code+2
    lda #<exec_ansi
    sta code_callback
    lda #>exec_ansi
    sta code_callback+1
    rts
.endproc

.proc esc_linefeed
    jsr cursor_down
    jmp cursor_show
.endproc

.proc esc_reverse_linefeed
    jsr cursor_up
    jmp cursor_show
.endproc

.proc esc_carriage_return
    jsr carriage_return
    jmp cursor_show
.endproc

.proc esc_reset
    lda p
    pha
    lda p+1
    pha
    jsr _term_init
    pla
    sta p+1
    pla
    sta p
    jmp term_reset
.endproc

ec_codes:
    .byte "["   ; Control sequence initator (CSI)
    .byte "D"   ; Linefeed
    .byte "E"   ; Newline
    .byte "M"   ; Reverse linefeed
    .byte "c"   ; Reset
    .byte 0
ec_hl:
    .byte <ansi_escape
    .byte <esc_linefeed
    .byte <esc_carriage_return
    .byte <esc_reverse_linefeed
    .byte <esc_reset
ec_hh:
    .byte >ansi_escape
    .byte >esc_linefeed
    .byte >esc_carriage_return
    .byte >esc_reverse_linefeed
    .byte >esc_reset

.proc exec_escape
    ldx #0
l:  lda ec_codes,x
    beq invalid
    cmp last_in
    beq found
    inx
    jmp l

invalid:
    lda #0  ; TODO: Reset in _term_put(). (pixel)
    sta code_callback+1
    jmp bell

found:
    lda #0
    sta code_callback+1
    lda ec_hl,x
    sta tmp
    lda ec_hh,x
    sta tmp+1
    jmp (tmp)
.endproc

.proc escape
    lda #<exec_escape
    sta code_callback
    lda #>exec_escape
    sta code_callback+1
    rts
.endproc

.proc do_nothing
    rts
.endproc

;;; Print char or process control colde.
.proc _term_put
    sta last_in

    ldx code_callback+1
    beq no_callback
    jmp (code_callback)
no_callback:

    ;; Handle control codes.
    jsr cursor_hide
    cmp #$1f
    bcs no_ctrl
    asl
    clc
    adc #<code_handlers
    sta j+1
    lda #>code_handlers
    adc #0
    sta j+2
    jsr j
    jmp cursor_show
j:  jmp ($f000)

no_ctrl:
    cmp #$7f
    bne print_char
    jmp delete

    ;; Print character.
print_char:
    ; Handle attribute 'reverse'.
    lda attributes
    and #ATTR_REVERSE
    bne reverse
    lda #0
    sta pencil_mode
    jmp do_char
reverse:
    lda #2
    sta pencil_mode

do_char:
    ; Draw char.
    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos
    lda last_in
    jsr putchar_fixed

    ; Handle attribute 'underline'.
    lda attributes
    and #ATTR_UNDERLINE
    beq no_underline
    lda xpos
    and #4
    bne underline_right
    lda #$f0
    bne underline ; (jmp)
underline_right:
    lda #$0f
underline:
    ldy #7
    eor (scr),y
    sta (scr),y
no_underline:

    ;; Move on.
    jsr cursor_step
    jmp cursor_show
.endproc

;;; Print string.
.proc term_puts
    jsr cursor_hide

l:  ldy #0
    lda (p),y
    beq r

    jsr _term_put

    inc p
    bne l
    inc p+1
    jmp l

r:  jmp cursor_show
.endproc

.proc _term_puts
    sta p
    stx p+1
    jmp term_puts
.endproc

;;; Print string of fixed length.
.proc _term_putsn
    sta tmp2
    jsr popax
    sta p
    stx p+1

    jsr cursor_hide

l:  ldy #0
    lda (p),y
    jsr _term_put
    dec tmp2
    beq r
    inc p
    bne l
    inc p+1
    jmp l

r:  jmp cursor_show
.endproc
