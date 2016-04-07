set_cbm_error:
    tax
    lda #<devcbm_errors
    sta s
    lda #>devcbm_errors
    sta @(++ s)
    ldy #0
l:  cpx #0
    beq +n
m:  lda (s),y
    jsr inc_s
    cmp #0
    bne -m
    dex
    jmp -l

n:  lda (s),y
    sta last_error
    jsr inc_s
    jsr devcon_print_string
    sec
    rts

return_cbm_error:
    jsr set_cbm_error
    jmp release

devcbm_errors:
    0            "No error." 0
    EMFILE       "Too many open files." 0
    EINVAL       "File is open." 0
    EINVAL       "File not open." 0
    ENOENT       "File not found." 0
    ENODEV       "Device not present." 0
    EINVAL       "File not input." 0
    EINVAL       "File not output." 0
    EINVAL       "Filename missing." 0
    ENODEV       "Illegal device." 0
    0 0 0 0 0
    0 0 0 0 0
    EBUSY        "No sector header." 0
    EBUSY        "No sync mark." 0
    EIO          "No sector data." 0
    EIO          "Checksum error." 0
    EIO          "Decode error." 0
    EIO          "Verify error." 0
    EACCES       "Write protected." 0
    EIO          "Checksum error." 0
    EIO          "Write overrun." 0
    EBUSY        "Disk ID mismatch." 0
    EINVAL       "Command not recognized." 0
    ENOSYS       "Command not implemented." 0
    EINVAL       "Command too long." 0
    EINVAL       "Invalid write filename." 0
    EINVAL       "No file given." 0
    0 0 0 0
    ENOENT       "System file not found." 0
    0 0 0 0 0
    0 0 0 0 0
    EACCES       "Invalid format." 0
    ESPIPE       "Record not present." 0
    ENOSPC       "Overflow in record." 0
    ENOSPC       "File too large." 0
    0 0 0 0
    0 0 0 0
    EBUSY        "Write file open." 0
    EINVAL       "File not open." 0
    ENOENT       "File not found." 0
    EEXIST       "File exists." 0
    EINVAL       "File type mismatch." 0
    ESPIPE       "No block." 0
    EINVAL       "Illegal track or sector." 0
    EIO          "Illegal system track or sector." 0
    0 0
    EBUSY        "No channel." 0
    EIO          "BAM error." 0
    ENOSPC       "Disk full." 0
    EACCES       "DOS version mismatch." 0
    ENODEV       "Drive not ready." 0
    EIO          "Format error." 0
    EINVAL       "llegal partition." 0
    EIO          "Bad system area." 0
