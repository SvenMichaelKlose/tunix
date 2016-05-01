main:
    ; Welcome the user.
    lda #<txt_booting
    ldy #>txt_booting
    jsr $cb1e

    jsr test_ultimem

    ;; Load core to block5.
    lda #<txt_loading_core
    ldy #>txt_loading_core
    jsr $cb1e

    lda #255
    sta $9ff2
    lda #BANK_CORE_CODE
    sta $9ffe

    lda #<path_core
    sta s
    lda #>path_core
    sta @(++ s)
    jsr gopen
    bcs +e
    lda #$00
    sta d
    lda #$a0
    sta @(++ d)
    jsr readm
    bcs +e
    jsr gclose

    ;; Load charset.
    lda #<txt_loading_charset
    ldy #>txt_loading_charset
    jsr $cb1e

    lda #BANK_DEVCON_CHARSET
    sta $9ff8

    lda #<path_charset
    sta s
    lda #>path_charset
    sta @(++ s)
    jsr gopen
    bcs +e2
    lda #$00
    sta d
    lda #$20
    sta @(++ d)
    jsr readm
    bcs +e
    jsr gclose

    ;; Get ready.
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    jmp $a000

e2: lda #<txt_cant_load_charset
    ldy #>txt_cant_load_charset
    jsr $cb1e
    jmp +l

e:  lda #<txt_cant_load_core
    ldy #>txt_cant_load_core
    jsr $cb1e
l:  jsr CHRIN
    jmp ($c000)

txt_booting:
    $93 @(ascii2petscii "BOOTING G...") 13 0

txt_loading_core:
    "LOADING CORE..." 13 0

txt_loading_charset:
    "LOADING CHARSET..." 13 0

txt_cant_load_core:
    "CANNOT LOAD 'CORE'." 13
    "EXITING..." 13 0

txt_cant_load_charset:
    "CANNOT LOAD 'CHARSET'." 13
    "EXITING..." 13 0

path_core:
    "CORE" 0

path_charset:
    "CHARSET" 0

take_over:
release:
start_task_switching:
stop_task_switching:
    rts

devcon_print_string:
    ldx s
    ldy @(++ s)
    jmp $cb1e
