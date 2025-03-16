; Turn LAMBDAs into interpreter-friendly
; QUASIQUOTE expressions.

(fn argnames (x)
  ;"Turn argument def into name list."
  (when x
    (. (? (atom x) x x.)
       (argnames .x))))

(def-tree-filter qq (x l a)
  (atom x) x
  (some-unquote? x)
    $(,x. ,(lamb .x. l x)))

(def-tree-filter lamb (x l a)
  (quote? x) x
  (binding-lambda? x)
    $((lambda ,(cadr x.)
        ,@(lamb (cddr x.) l
              (+ (argnames .x.) a))))
  (lambda? x)
    $(lambda ,.x.
       ,@(lamb ..x l
             (+ (argnames .x.) a)))
  (member x a)
    $(quote ,x)
  (quasiquote? x)
    (qq (++ l) x))

; Map arguments, turn child LAMBDAs
; into QUASIQUOTES.
(fn enscope (fndecl)
  $(fn ,.fndecl. ,..fndecl.
     ,@(lamb ...fndecl 0
           (argnames .fndecl.))))
