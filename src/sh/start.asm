program_size = @(- program_end program_start)

    $00 $20
    <program_size >program_size

    org $2000
program_start:
