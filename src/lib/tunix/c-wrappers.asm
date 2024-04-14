.import lib_mode
.import lib_schedule, lib_getpid
.import lib_fork, lib_suspend
.import lib_resume, lib_exit, lib_kill
.import lib_wait
.import lib_iopage_alloc
.import lib_iopage_commit
.import lib_iopage_free
.import lib_proc_list, lib_proc_info
.import popax

.export _tunix_mode = lib_mode
.export _tunix_schedule = lib_schedule
.export _tunix_getpid = lib_getpid
.export _tunix_fork = lib_fork
.export _tunix_kill;
.export _tunix_exit = lib_exit
.export _tunix_wait = lib_wait
.export _tunix_suspend = lib_suspend
.export _tunix_resume = lib_resume
.export _tunix_proc_list = lib_proc_list
.export _tunix_proc_info = lib_proc_info
.export _tunix_iopage_alloc = lib_iopage_alloc
.export _tunix_iopage_commit = lib_iopage_commit
.export _tunix_iopage_free = lib_iopage_free

; A: process ID
; X: exit code
.proc _tunix_kill
    jsr popax
    jmp lib_kill
.endproc
