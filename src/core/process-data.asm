FILE_OPENED     = 128
FILE_STREAM     = 64    ; Otherwise block–oriented.
FILE_READABLE   = 1
FILE_WRITABLE   = 2

    data

    org $0400

0 0 0       ; JMP to link().


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Information in master core 0. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This area is not used in other banks than #0.

core_data_start = @*pc*


;;; Processes

process_states:         fill max_num_processes
process_cores:          fill max_num_processes
process_cores_saved:    fill max_num_processes


;;; Banks allocated by "alloc_block":
master_banks:       fill @(/ 1024 8)


;;; Virtual files which can be shared by processes.
vfile_states:       fill max_num_vfiles ; Like in file_states.
vfile_parents:      fill max_num_vfiles ; (Parent) directory.
vfile_ops_l:        fill max_num_vfiles
vfile_ops_h:        fill max_num_vfiles
vfile_handles:      fill max_num_vfiles ; Within drivers.
vfile_refcnts:      fill max_num_vfiles ; Number of processes using vfiles.
vfile_root:         0


;;; /dev/cbm
devcbm_logical_file_numbers:    fill 8


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Per–process information in each copy of the core. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

per_process_data_size = @(- per_process_data_end per_process_data_start)
per_process_data_start:

;;; Task state.
; +3K area is stored in process_cores_saved in the master core.
saved_pc:           0 0
saved_a:            0
saved_x:            0
saved_y:            0
saved_flags:        0
saved_sp:           0
saved_stack:        fill 256
saved_zeropage:     fill $90    ; BASIC part only.
saved_bank_ram:     0
saved_bank_io:      0
saved_bank1:        0
saved_bank2:        0
saved_bank3:        0
saved_bank5:        0


parent_process: 0
program_start:  0 0
bss_size:       0 0
process_slot:   0       ; Indexes in master core tables.


;;; File system

file_states:    fill max_num_files_per_process ; States of opened files.
file_vfiles:    fill max_num_files_per_process ; Indexes into virtual files in master core.
file_positions_0: fill max_num_files_per_process
file_positions_1: fill max_num_files_per_process
file_positions_2: fill max_num_files_per_process
file_positions_3: fill max_num_files_per_process

path_component: fill @(++ max_file_name_length)

;; Working directory vfile.
pwd:        0

;; Flags for fs_open().
fs_mode:    0

;;; Banks allocated by "alloc_block":
banks:      fill @(/ 1024 8)


;;; Libraries
num_libraries:      0
libraries:          fill max_num_libraries_per_process

end_of_library_calls: 0 0
library_calls:

per_process_data_end:

    end
