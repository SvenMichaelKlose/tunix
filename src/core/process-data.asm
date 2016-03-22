    data

;;; Information init process' core.

current_process:    0   ; Index into following tables.
process_states: fill max_num_processes
process_cores:  fill max_num_processes


;;; Per–process information in each core bank.

bank_ram:   0
bank_io:    0
bank1:      0
bank2:      0
bank3:      0
bank5:      0

banks:      fill @(/ 1024 8)

saved_pc:           0 0
saved_a:            0
saved_x:            0
saved_y:            0
saved_flags:        0
saved_sp:           0
saved_stack:        fill 256
saved_zeropage:     fill $90    ; BASIC part only.
saved_blk_io:       0
saved_blk1:         0
saved_blk2:         0
saved_blk3:         0
saved_blk5:         0

program_start:      0 0
process_slot:       0

num_libraries:      0
library_cores:      fill max_num_libraries_per_process

end_of_library_calls: 0 0
library_calls:

    end
