(code %jmp-nil (p d)
  (hbz)
  (beq d))

(code %jmp-t (p d)
  (hbz)
  (bne d))
