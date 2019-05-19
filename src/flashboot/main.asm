.export main
.exportzp s, d, c, tmp
.importzp name, namelen

.import ultifs_enter_root, ultifs_enter, ultifs_load

.zeropage

s:              .res 2
d:              .res 4
c:              .res 2
tmp:            .res 1

.segment "STARTUP"

.proc main
    ; Don't get interrupted.
    sei
    lda #$7f
    sta $911d
    sta $911e

    cld
    ldx #$ff
    txs

    lda #$22    ; Red screen.
    sta $900f
    lda #$00    ; Blank screen.
    sta $9002

    jsr $fd8d   ; Init memory.
    jsr $fd52   ; Init KERNAL.
    jsr $fdf9   ; Init VIAs.
    jsr $e518   ; Init VIC.

    lda #$7f    ; Yellow screen.
    sta $900f

    cli

    jsr ultifs_enter_root

    ; Enter directory 'g'.
    lda #<fn_ingle
    sta name
    lda #>fn_ingle
    sta name+1
    lda #fn_ingle_end-fn_ingle
    sta namelen
    jsr ultifs_enter

    lda #$00
    sta d
    lda #$20
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_desktop
    sta name
    lda #>fn_desktop
    sta name+1
    lda #fn_desktop_end-fn_desktop
    sta namelen
    jsr ultifs_load

    lda #$00
    sta d
    lda #$40
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_core
    sta name
    lda #>fn_core
    sta name+1
    lda #fn_core_end-fn_core
    sta namelen
    jsr ultifs_load

    lda #$00
    sta d
    lda #$a0
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_ultifs
    sta name
    lda #>fn_ultifs
    sta name+1
    lda #fn_ultifs_end-fn_ultifs
    sta namelen
    jsr ultifs_load

    lda #$00
    sta d
    lda #$c0
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_charset4x8
    sta name
    lda #>fn_charset4x8
    sta name+1
    lda #fn_charset4x8_end-fn_charset4x8
    sta namelen
    jsr ultifs_load

    ; Load file window.
    lda #$00
    sta d
    lda #$e0
    sta d+1
    lda #$00
    sta d+2
    sta d+3
    lda #<fn_file_window
    sta name
    lda #>fn_file_window
    sta name+1
    lda #fn_file_window_end-fn_file_window
    sta namelen
    jsr ultifs_load

    ; Run it.
    lda #$5d    ; Green screen.
    sta $900f
    jsr init_ram_banks
    jmp $4000
.endproc

.proc dummy_link
    rts
.endproc

.proc init_ram_banks
    ; Activate all RAM.
    lda #%00111111
    sta $9ff1
    lda #%01111111
    sta $9ff2
    lda #0
    tax
    stx $9ff4
    sta $9ff5
    stx $9ff6
    sta $9ff7
    inx
    stx $9ff8
    sta $9ff9
    inx
    stx $9ffa
    sta $9ffb
    inx
    stx $9ffc
    sta $9ffd
    rts
.endproc

txt_not_found:
    .byte "FILE NOT FOUND.", 0

fn_ingle:
    .byte "ingle"
fn_ingle_end:

fn_core:
    .byte "desktop-lib.bin"
fn_core_end:

fn_desktop:
    .byte "desktop.bin"
fn_desktop_end:

fn_ultifs:
    .byte "desktop-ultifs.bin"
fn_ultifs_end:

fn_charset4x8:
    .byte "charset-4x8.bin"
fn_charset4x8_end:

fn_file_window:
    .byte "desktop-file-window.bin"
fn_file_window_end:
