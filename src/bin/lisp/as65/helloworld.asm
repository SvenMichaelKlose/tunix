(= *as65-pc* (basic-start))

load_address:
    (word (basic-start))
    (word basic_end)
    $01 $00 ; line number
    $9e     ; SYS token
    (unless (first-pass?)
      (word main))
    0       ; line end
basic_end:
    $00 $00
