 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;                                ;;;
 ;;; ###### ##  ## ####   ## ##  ## ;;;
 ;;;   ##   ##  ## ##  ## ##   ##   ;;;
 ;;;   ##   ###### ##  ## ## ##  ## ;;;
 ;;;                                ;;;
 ;;; Multi-tasking KERNAL extension ;;;
 ;;;  (Commodore VIC-20 + UltiMem)  ;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Convenience wrappers for system calls.

__VIC20__       = 1
TUNIX_DEVICE    = 31

.include "cbm_kernal.inc"

.ifdef DEBUG

.export cmd_fork, cmd_exit, cmd_kill
.export cmd_wait, cmd_getpid
.export cmd_proc_info

.endif ; .ifdef DEBUG

        .data

cmd_fork:           .byte "PF"
cmd_exit:           .byte "PE"
cmd_kill:           .byte "PKc"
cmd_wait:           .byte "PW"
cmd_getpid:         .byte "P"
cmd_iopage_alloc:   .byte "DA"
cmd_iopage_commit:  .byte "DC"
cmd_iopage_free:    .byte "DF"
cmd_proc_list:      .byte "PL"
cmd_proc_info:      .byte "PI"

        .code

.export lib_schedule
.proc lib_schedule
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #0
    jsr SETNAM
    jmp OPEN
.endproc

.export lib_getpid
.proc lib_getpid
    ;; Check if back in init.
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #1
    ldx #<cmd_getpid
    ldy #>cmd_getpid
    jsr SETNAM
    jmp OPEN
.endproc

.export lib_fork
.proc lib_fork
    ;; Fork and wait for child to exit.
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_fork
    ldy #>cmd_fork
    jsr SETNAM
    jmp OPEN
.endproc

; A: Exit code.
.export lib_exit
.proc lib_exit
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_exit
    ldy #>cmd_exit
    jsr SETNAM
    jmp OPEN
.endproc

; A: process ID
; X: exit code
.export lib_kill
.proc lib_kill
    stx cmd_kill+2
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #3
    ldx #<cmd_kill
    ldy #>cmd_kill
    jsr SETNAM
    jmp OPEN
.endproc

; A: process ID
.export lib_wait
.proc lib_wait
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_wait
    ldy #>cmd_wait
    jsr SETNAM
    jmp OPEN
.endproc

; Print process info.
; A: process ID
.export lib_proc_list
.proc lib_proc_list
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_proc_list
    ldy #>cmd_proc_list
    jsr SETNAM
    jmp OPEN
.endproc

; Print process info.
; A: process ID
.export lib_proc_info
.proc lib_proc_info
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_proc_info
    ldy #>cmd_proc_info
    jsr SETNAM
    jmp OPEN
.endproc

; Allocate I/O page
; Returns:
;  A: Page number
.export lib_iopage_alloc
.proc lib_iopage_alloc
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_iopage_alloc
    ldy #>cmd_iopage_alloc
    jsr SETNAM
    jmp OPEN
.endproc

; Commit I/O page
;  A: Page number
.export lib_iopage_commit
.proc lib_iopage_commit
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_iopage_commit
    ldy #>cmd_iopage_commit
    jsr SETNAM
    jmp OPEN
.endproc

; Free I/O page
;  A: Page number
.export lib_iopage_free
.proc lib_iopage_free
    tay
    lda #TUNIX_DEVICE
    tax
    jsr SETLFS
    lda #2
    ldx #<cmd_iopage_free
    ldy #>cmd_iopage_free
    jsr SETNAM
    jmp OPEN
.endproc
