loaded_kernal:
    org $a000

kernal:
    @(syscall-table)
