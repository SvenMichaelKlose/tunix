init:
    ;; Mount root file system.
    ; Print message.
    lda #<txt_mounting_root
    sta s
    lda #>txt_mounting_root
    sta @(++ s)
    jsr devcon_print_string

    ; Make vfile.
    jsr devcbm_make_root

    ; Correct path working directory for this process.
    ldy $9ff4
    lda #0
    sta $9ff4
    lda vfile_root
    sty $9ff4
    sta pwd

    ;; Start shell.
    ; Print message.
    lda #<txt_starting_sh
    sta s
    lda #>txt_starting_sh
    sta @(++ s)
    jsr devcon_print_string

    ; Load sh.
    lda #<path_sh
    sta s
    lda #>path_sh
    sta @(++ s)
    lda #1          ; Wait until process finishes.
    jsr launch

    ;; Shutdown g.
    ; Print message.
    lda #<txt_shutdown
    sta s
    lda #>txt_shutdown
    sta @(++ s)
    jsr devcon_print_string

    ;; Start GUI.
    lda #<ui_symbols
    sta s
    lda #>ui_symbols
    sta @(++ s)
    lda #<ui_jumps
    sta d
    lda #>ui_jumps
    sta @(++ d)
    jsr $400

    jsr boot_ui

    ; Wait forever.
w:  jmp -w

txt_mounting_root:
    "Mounting root file system..." 10 0

txt_shutdown:
    "g shut down. Have a nice day!" 0

txt_starting_sh:
    "Starting sh..." 10 10 0

path_sh:
    @(ascii2petscii "SH") 0

ui_symbols:
    "GFX" 0
    "boot" 0
    0

ui_jumps:
boot_ui:    jmp $0000
