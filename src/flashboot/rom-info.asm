    org $a000

    ; ROM autostart info.
    <main >main     ; Cold start vector
    <main >main     ; Warm start vector
    "A0"
    $c3 $c2 $cd     ; "CBM"
