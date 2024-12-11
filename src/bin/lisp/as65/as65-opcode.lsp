(= *alv?* t) ; Verbose AUTOLOAD.

(var *mn-6502*
  '((nil bit jmp jmpi sty ldy cpy cpx)
    (ora and eor adc sta lda cmp sbc)
    (asl rol lsr ror stx ldx dec inc)))

(var *am-6502*
  '((imm  zp nil abs nil  zpx nil  absx)
    (izpx zp imm abs izpy zpx absy absx)
    (imm  zp nil abs nil  zpx nil  absx)))

(var *6502*
 '((;(nil bit jmp jpi sty ldy cpy cpx)
     (brk jsr rti rts nil  t   t   t ) ; immediate
     (nil  t  nil nil  t   t   t   t ) ; zero page
     (php plp pha pla dey tay iny inx) ; implied
     (nil  t   t   t   t   t   t   t ) ; absolute
     (bpl bmi bvc bvs bcc bcs bne beq) ; branch
     (nil nil nil nil  t   t)          ; zero page X
     (clc sec cli sei tya clv cld sed) ; implied
     (nil nil nil nil  t   t))         ; absolute X
   (;(ora and eor adc sta lda cmp sbc)
     t                                 ; indirect X
     t                                 ; zero page
     ( t   t   t   t  nil  t   t   t ) ; immediate
     t                                 ; absolute
     t                                 ; indirect Y
     t                                 ; zero page X
     t                                 ; absolute Y
     t)                                ; absolute X
   (;(asl rol lsr ror stx ldx dec inc)
     (nil nil nil nil nil  t)          ; immediate
     t                                 ; zero page
     ( t   t   t   t  txa tax dex nop) ; implied
     t                                 ; absolute
     nil                               ; implied
     t                                 ; zero page X (Y for LDX and STX)
     (nil nil nil nil txs tsx)         ; implied
     t)))                              ; absolute X (Y for LDX and STX)

(fn as65/mn-cc (mn)
  (dotimes (cc 3)
    (and (member mn (nth cc *mn-6502*))
         (return cc))))

(fn as65/adjust-am (mn am)
  ; "LDX/STX: Change addressing modes from ABSY/ZPY to ABSX/ZPX."
  (? (member mn '(ldx stx))
     (?
       (eq am 'absy) 'absx
       (eq am 'zpy)  'zpx
       am)
     am))

(fn as65/mnam-opc (mn am)
  ; "Get opcode of 1st class instruction."
  ; Get CC by mnemonic.
  (let-when cc (as65/mn-cc mn)
    (= am (as65/adjust-am mn am))
    ; Get AAA by mnemonic.
    (let-when aaa (position mn (nth cc *mn-6502*))
      ; Get BBB by addressing mode.
      (let-when bbb (position am (nth cc *am-6502*))
        ; Check if opcode is legal.
        (when (!= (nth bbb (nth cc *6502*))
                (or (eq t !)
                    (eq t (nth aaa !))))
          (list aaa bbb cc))))))

(fn as65/mnimm-opc (mn)
  ; "Get opcode of 2nd class instruction."
  (block found
    ; Scan all CC pages.
    (dolist-indexed (c cc *6502*)
      ; Scan all its BBB addressing modes.
      (dolist-indexed (b bbb c)
        ; Check for mnemonic in AAA row.
        (unless (eq t b) ; (all of row 1st class instructions)
          (let-when aaa (position mn b)
            ; Mnemonic found.
            (return (list aaa bbb cc) found)))))))

(fn as65/opcode (mn am)
  ; "Make opcode parts (AAA, BBB, CC) from mnemonic and addressing mode."
  (assert (member am *am-6502*) "Unknown address mode " am)
  (awhen (? am
            (as65/mnam-opc mn am)
            (as65/mnimm-opc mn))
    (+ (+ (<< (car !) 5)
          (<< (cadr !) 2))
       (caddr !))))

(fn as65/zpconv (op am zam)
  (? op
     (? (< op 256) zam am)
     am))

(fn as65/inst (i)
  (with (mode (cdr (assoc 'mode i))
         mnem (cdr (assoc 'mnem i))
         ireg (cdr (assoc 'ireg i))
         op   (cdr (assoc 'op i)))
    (as65/opcode
        mnem
        (case mode
          abs (?
                (eq 'x ireg) (as65/zpconv op 'absx 'zpx)
                (eq 'y ireg) (as65/zpocnv op 'absy 'zpy)
                abs)
          ind (?
                (eq 'x ireg) 'izpx
                (eq 'y ireg) 'izpy
                ind)
          mode))))
