(load 'as65/package.lsp)
(require 'let 'aprog1 'let2 'dup 'case); 'aprog1)

(fn zpconv (op am zam)
  (? op
     (? (< op 256) zam am)
     am))

(fn asm-inst (i)
  ; "Asssemble parsed instruction."
  (let (mode (cdr (assoc 'mode i))
        mnem (cdr (assoc 'mnem i))
        ireg (cdr (assoc 'ireg i))
        op   (cdr (assoc 'op i)))
    (opcode
        mnem
        (case mode
          'abs (case ireg
                 'x (zpconv op 'absx 'zpx)
                 'y (zpocnv op 'absy 'zpy)
                 'abs)
          'ind (case ireg
                 'x 'izpx
                 'y 'izpy
                 'ind)
          mode))))

(fn asm (x first-pass?)
  (!= (parse x)
    (. !.
       (case (cdr (assoc 'type .!))
         'inst   (asm-inst .!)
         'label  nil
         'expr   nil
         'data   nil
         (error "Bad syntax: " x)))))

(in-package nil)
