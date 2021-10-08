.include "/usr/local/share/cc65/asminc/cbm_kernal.inc"

.export _term_init, _term_put, _term_puts

.import init_bitmap_mode, gfx_init
.import clear_screen
.import box
.import putchar_fixed
.import _pattern_empty
.import _pattern_solid
.import init_region_stack
.import reset_region
.import moveram
.import copy_area
.import calcscr
.import charset, charset_size

.importzp s, d, c, scrbase, scr
.importzp font, pencil_mode, pattern
.importzp xpos, ypos, xpos2, ypos2, width, height
.importzp screen_width, screen_height, screen_columns

    .zeropage

tmp:            .res 2
tmp2:           .res 2
p:              .res 2

    .data

cursor_x:       .res 1
cursor_y:       .res 1
has_cursor:     .res 1
visible_cursor: .res 1
code:           .res 2
code_length:    .res 1
code_callback:  .res 2
attributes:     .res 2

    .code

.proc _term_init
    jsr clear_screen
    lda #1
    ldx #0
    ldy #2
    jsr init_bitmap_mode
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

    ldy #0
    sty cursor_x
    sty cursor_y
    sty visible_cursor
    iny
    sty has_cursor
    ldy #$ff
    sty code_length

    lda #%00010000
    sta attributes
    jmp cursor_draw
.endproc

.proc scroll_up
    lda #<charset
    sta s
    lda #>charset
    sta s+1

    ldx #screen_columns
m:  ldy #7
    lda #0
l:  sta (s),y
    dey
    bpl l

    lda s
    clc
    adc #screen_height
    sta s
    bcc n
    inc s+1
n:  dex
    bne m

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

    ldx #7
    lda #0
l2: sta charset + (screen_height * screen_columns) - 8,x
    dex
    bpl l2

    rts
.endproc

.proc cursor_draw
    lda attributes
    and #%00010000
    beq r

    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos
    lda #<_pattern_solid
    sta pattern
    lda #>_pattern_solid
    sta pattern+1
    lda #2
    sta pencil_mode
    asl
    tax
    dex
    stx width
    asl
    sta height
    jmp box

r:  rts
.endproc

.proc cursor_hide
    pha
    lda visible_cursor
    inc visible_cursor
    ora #0
    bne r
    jsr cursor_draw
r:  pla
    rts
.endproc

.proc cursor_show
    pha
    dec visible_cursor
    bne r
    jsr cursor_draw
r:  pla
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
    cmp #screen_height-8
    bne n
    jmp scroll_up

n:  clc
    adc #8
    sta cursor_y
    
    rts
.endproc

.proc carriage_return
    lda #0
    sta cursor_x
    rts
.endproc

.proc exec_cursor_motion
    lda code+1
    asl
    asl
    sta cursor_x
    lda code
    asl
    asl
    asl
    sta cursor_y
    jmp cursor_show
.endproc

.proc init_cursor_motion
    lda #1
    sta code_length
    lda #<exec_cursor_motion
    sta code_callback
    lda #>exec_cursor_motion
    sta code_callback+1
    rts
.endproc

.proc beep
    ldy #$10
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
    lda #0
    sta cursor_x
    sta xpos

    lda cursor_y
    cmp #screen_height-8
    beq done

    sta ypos

    lda #screen_height-8-1
    sec
    sbc ypos
    sta height

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
    lda #0
    sta cursor_x

    lda cursor_y
    cmp #screen_height-8
    beq done

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
    lda cursor_y
    pha
    lda #screen_height-8
    sta cursor_y
    jsr clear_to_eol
    pla
    sta cursor_y
    rts
.endproc

.proc escape
    lda #0
    sta code_length
    lda #<exec_escape
    sta code_callback
    lda #>exec_escape
    sta code_callback+1
    rts
.endproc

.proc enable_attribute
    lda code
    ora attributes
    sta attributes
    jmp cursor_show
.endproc

.proc disable_attribute
    lda code
    eor #$ff
    and attributes
    sta attributes
    jmp cursor_show
.endproc

.proc exec_escape
    lda code

    cmp #$0b
    bne n1
    lda #0
    sta code_length
    lda #<enable_attribute
    sta code_callback
    lda #>enable_attribute
    sta code_callback+1
    jmp cursor_show
n1:

    cmp #$0c
    bne n2
    lda #0
    sta code_length
    lda #<disable_attribute
    sta code_callback
    lda #>disable_attribute
    sta code_callback+1

n2:
    jmp cursor_show
.endproc

; Output control codes:
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

.proc _term_put
    jsr cursor_hide

    ldx code_length
    bmi no_code

    sta code,x
    dec code_length
    bpl r
    jmp (code_callback)

no_code:

; 01,x,y:   Cursor motion
    cmp #$01
    bne n7
    jsr init_cursor_motion
r:  jmp cursor_show
n7:

; 02:       Insert line
    cmp #$02
    bne n12
    jsr insert_line
    jmp cursor_show
n12:

; 03:       Delete line
    cmp #$03
    bne n13
    jsr delete_line
    jmp cursor_show
n13:

; 07:       BEL: beep and/or flash screen
    cmp #$07
    bne n6
    jsr beep
    jmp cursor_show
n6:

; 08:       BS; Backspace
    cmp #$08
    bne n3
    jsr cursor_left
    jmp cursor_show
n3:

; 09:       HT; Horizontal tab
    cmp #$09
    bne n10
    jsr tab
    jmp cursor_show
n10:

; 0a:       LF: Line feed
; 0b:       VF: Vertical tab
    cmp #$0a
    beq line_feed
    cmp #$0b
    bne n
line_feed:
    jsr cursor_down
    jmp cursor_show
n:  

; 0c:       FF: Form feed, Clear screen
; 1a:       Clear screen
    cmp #$0c
    beq form_feed
    cmp #$1a
    bne n4
form_feed:
    jsr clear_screen
    lda #0
    sta cursor_x
    sta cursor_y
    jmp cursor_show
n4:

; 0d:       CR: Carriage return
    cmp #$0d
    bne n2
    jsr carriage_return
    jmp cursor_show
n2:

; 1b:       ESC: Escape
    cmp #$1b
    bne n14
    jsr escape
    jmp cursor_show
n14:

; 18:       Clear to EOL
    cmp #$18
    bne n9
    jsr clear_to_eol
    jmp cursor_show
n9:

; 1e:       Home
    cmp #$1e
    bne n5
    lda #0
    sta cursor_x
    sta cursor_y
    jmp cursor_show
n5:

; 7f:       DEL: BS, ' ', BS
    cmp #$7f
    bne n8
    lda #7
    jsr _term_puts
    lda #32
    jsr _term_puts
    lda #7
    jsr _term_puts
    jmp cursor_show
n8:

    pha
    lda cursor_x
    sta xpos
    lda cursor_y
    sta ypos
    pla
    jsr putchar_fixed
    jsr cursor_step
    jmp cursor_show
.endproc

.proc putstring_fixed
    jsr cursor_hide

l:  ldy #0
    lda (p),y
    beq r

    jsr _term_put

    inc p
    bne l
    inc p+1
    jmp l   ; (bne)

r:  jmp cursor_show
.endproc

.proc _term_puts
    sta p
    stx p+1
    jmp putstring_fixed
.endproc

    .data

    .align 256

our_charset:
    .include "charset-4x8.asm"
