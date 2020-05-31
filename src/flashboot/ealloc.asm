; Extended memory allocator
;
; Can handle up to 256 blocks of max 8191 bytes each.
;
; Allocated and free blocks are maintained in two
; doubly-linked lists in RAM1,2,3.  Each block is
; prefixed by the index of a block.

MAX_EALLOC_BLOCKS = 256

.bss

num_blocks:         .res 2
first_free:         .res 2
first_allocated:    .res 2
sizes:              .res MAX_EALLOC_BLOCKS
prev_l:             .res MAX_EALLOC_BLOCKS
prev_h:             .res MAX_EALLOC_BLOCKS
next_l:             .res MAX_EALLOC_BLOCKS
next_h:             .res MAX_EALLOC_BLOCKS

.code

.proc ealloc_init
    lda #0
    sta first_free
    sta first_free
.endproc
