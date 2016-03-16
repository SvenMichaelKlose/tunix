    data
bank_ram:   0 0
bank1:      0 0
bank2:      0 0
bank3:      0 0
bank_io2:   0 0
bank_io3:   0 0
bank5:      0 0

banks:
    %00111111
    fill 255

saved_stack:
    fill 256

; Copy of what would be the BASIC part of the zero page.
saved_zeropage:
    fill $a0

    end
