.import program_end
.import boot, calcscr, setpattern, vline, hline, frame, box, putchar, putchar_fixed, putstring, copy_area

symbol_index_size = symbol_index_end-symbol_index_start

    .word symbol_index_size

symbol_index_start:
    .asciiz "boot" 
    .word boot
    .asciiz "calcscr" 
    .word calcscr
    .asciiz "setpattern" 
    .word setpattern
    .asciiz "vline" 
    .word vline
    .asciiz "hline" 
    .word hline
    .asciiz "frame" 
    .word frame
    .asciiz "box" 
    .word box
    .asciiz "putchar" 
    .word putchar
    .asciiz "putchar_fixed" 
    .word putchar_fixed
    .asciiz "putstring" 
    .word putstring
    .asciiz "copy_area" 
    .word copy_area
    .byte 0
symbol_index_end:

program_size = program_end-program_start

    .word program_start
    .word program_size
    .word 0         ; Data size.

.org $2000
program_start:
