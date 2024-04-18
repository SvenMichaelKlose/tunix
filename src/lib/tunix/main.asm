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

.export cmd_mode
.export cmd_alloc, cmd_free
.export cmd_fork, cmd_exit, cmd_kill
.export cmd_suspend, cmd_release
.export cmd_wait, cmd_getpid
.export cmd_proc_info

.endif ; .ifdef DEBUG

        .data

cmd_mode:           .byte "GM"
cmd_alloc:          .byte "MA"
cmd_free:           .byte "MF"
cmd_fork:           .byte "PF"
cmd_exit:           .byte "PE"
cmd_suspend:        .byte "PS"
cmd_resume:         .byte "PR"
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

.macro syscall0 name, cmd, len
    .export name
    .proc name
        lda #TUNIX_DEVICE
        tax
        jsr SETLFS
        lda #len
        ldx #<cmd
        ldy #>cmd
        jsr SETNAM
        jmp OPEN
    .endproc
.endmacro

.macro syscall1 name, cmd, len
    .export name
    .proc name
        tay
        lda #TUNIX_DEVICE
        tax
        jsr SETLFS
        lda #len
        ldx #<cmd
        ldy #>cmd
        jsr SETNAM
        jmp OPEN
    .endproc
.endmacro

.macro syscall2 name, cmd, len
    .export name
    .proc name
        stx cmd+2
        tay
        lda #TUNIX_DEVICE
        tax
        jsr SETLFS
        lda #len
        ldx #<cmd
        ldy #>cmd
        jsr SETNAM
        jmp OPEN
    .endproc
.endmacro

syscall1 lib_mode,          cmd_mode, 2
syscall0 lib_alloc,         cmd_alloc, 2
syscall1 lib_free,          cmd_free, 2
syscall0 lib_getpid,        cmd_getpid, 1
syscall0 lib_fork,          cmd_fork, 2
syscall1 lib_exit,          cmd_exit, 2
syscall2 lib_kill,          cmd_kill, 3
syscall1 lib_suspend,       cmd_suspend, 2
syscall1 lib_resume,        cmd_resume, 2
syscall1 lib_wait,          cmd_wait, 2
syscall1 lib_proc_list,     cmd_proc_list, 2
syscall1 lib_proc_info,     cmd_proc_info, 2
syscall0 lib_iopage_alloc,  cmd_iopage_alloc, 2
syscall1 lib_iopage_commit, cmd_iopage_commit, 2
syscall1 lib_iopage_free,   cmd_iopage_free, 2
