(load 'as65/package.lsp)

(fn zpconv (op am zam)
  (? op
     (? (< op 256) zam am)
     am))

(fn asm0 (i) 
  ; "Asssemble parsed instruction."
  (with (mode (cdr (assoc 'mode i))
         mnem (cdr (assoc 'mnem i))
         ireg (cdr (assoc 'ireg i))
         op   (cdr (assoc 'op i))) 
    (opcode
        mnem
        (case mode
          abs (?
                (eq 'x ireg)   
                  (zpconv op 'absx 'zpx)
                (eq 'y ireg)
                  (zpocnv op 'absy 'zpy)
                abs)
          ind (?
                (eq 'x ireg) 'izpx
                (eq 'y ireg) 'izpy
                ind)
          mode))))

(fn asm (x first-pass?)
  (!= (parse x)
    (?
      (assoc 'mnem !)
        (asm0 !)
      (assoc 'label !)
        nil
      (assoc 'expr !)
        nil
      (assoc 'data !)
        nil
      (error "Bad syntax: " x))))

(in-package nil)
