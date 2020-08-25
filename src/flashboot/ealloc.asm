; Extended memory allocator
;
; Can handle up to 256 blocks of max 8191 bytes each.
;
; Allocated and free blocks are maintained in two
; doubly-linked lists in RAM1,2,3.  Each block is
; prefixed by the index of a block.
;
; Allocated blocks are not mapped to memory.  Instead
; data has to be transferred with 'ecopy_to' and 'e_copy_from'.

.export ealloc_init
.export ealloc
.export efree
.export ecopy_to
.export ecopy_from

MAX_EALLOC_BLOCKS = 256

.bss

num_blocks:         .res 2
first_free:         .res 1
first_allocated:    .res 1
sizes:              .res MAX_EALLOC_BLOCKS
prev_l:             .res MAX_EALLOC_BLOCKS
prev_h:             .res MAX_EALLOC_BLOCKS
next_l:             .res MAX_EALLOC_BLOCKS
next_h:             .res MAX_EALLOC_BLOCKS
ptr_l:              .res MAX_EALLOC_BLOCKS
ptr_h:              .res MAX_EALLOC_BLOCKS

.code

.proc ealloc_init
    ldx #0
    stx first_free
    rts
.endproc

.proc ealloc
    rts
.endproc

.proc efree
    rts
.endproc

.proc ecopy_to
    rts
.endproc

.proc ecopy_from
    rts
.endproc
