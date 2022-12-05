.exportzp s, d, c, tmp
.exportzp _free_banks, _alloc_banks

    .zeropage

s:      .res 2
d:      .res 2
c:      .res 2
tmp:    .res 2
_free_banks:    .res 2
_alloc_banks:   .res 2
