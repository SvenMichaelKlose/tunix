(load 'compiler/package.lsp)
(require 'let 'do)

(fn %block? (x)
  (and (cons? x)
       (eq '%block x.)))

(fn fold-block (x)
  ; "Splice rest of %BLOCKs."
  (? (%block? x)
     (!= .x
       (? (cons? !)
          (mapcan fold-block !)
          (list !)))
     (list x)))

(in-package nil)
