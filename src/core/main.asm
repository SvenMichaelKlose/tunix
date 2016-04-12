main:
if @*rom?*
    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init VIC.
end

    ; Welcome the user.
    lda #<txt_booting
    ldy #>txt_booting
    jsr $cb1e

    jsr test_ultimem

if @(not *rom?*)
stop:
    ;; Load core to block5.
    lda #255
    sta $9ff2
    lda #2
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

    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff

    jmp $a000

e:  ldx #<txt_cant_open
    ldy #>txt_cant_open
    jsr $cb1e
    jsr CHRIN
    jmp ($c000)
end

if @(not *rom?*)
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff

    jmp $a000
end

txt_booting:
    $93 @(ascii2petscii "BOOTING G...") 13 0

if @(not *rom?*)
txt_cant_open:
    "CANNOT LOAD 'CORE'." 13
    "EXITING..." 13 0

path_core:
    "CORE" 0

take_over:
release:
start_task_switching:
stop_task_switching:
    rts

devcon_print_string:
    ldx s
    ldy @(++ s)
    jmp $cb1e
end
