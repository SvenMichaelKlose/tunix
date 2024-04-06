.import lib_schedule, lib_getpid, lib_fork
.import lib_exit, lib_kill, lib_wait
.import lib_proc_list, lib_proc_info
.import popax

.export _tunix_schedule = lib_schedule
.export _tunix_getpid = lib_getpid
.export _tunix_fork = lib_fork
.export _tunix_exit = lib_exit
.export _tunix_wait = lib_wait
.export _tunix_suspend = lib_wait
.export _tunix_resume = lib_wait
.export _tunix_proc_list = lib_proc_list
.export _tunix_proc_info = lib_proc_info

; A: process ID
; X: exit code
.proc _tunix_kill
    jsr popax
    jmp lib_kill
.endproc
