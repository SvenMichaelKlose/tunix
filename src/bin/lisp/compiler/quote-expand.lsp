(in-package compiler/qex
  '(any-qq? enquote enquote-r
    enqq enqqs backq disp r))

(fn any-quasiquote? (x)
  (or (quasiquote? x)
      (quasiquote-splice? x)))

(fn enquote (x)
  (? (constant-literal? x)
     x
     $(quote ,x)))

(fn enquote-r (x)
  (? (atom x)
     (enquote x)
     $(. ,(enquote-r x.)
         ,(enquote-r .x))))

(fn enqq (x)
  (? (any-quasiquote? (cadr x.))
     $(. ,(backq (cadr x.))
         ,(backq .x))
     $(. ,(cadr x.)
         ,(backq .x))))

(fn enqqs (x)
  (? (any-quasiquote? (cadr x.))
     (error "Illegal ~A as argument to ,@ (QUASIQUOTE-SPLICE)." (cadr x.))
     (with-gensym g
       ; TODO: Make TRANSPILER-MACROEXPAND work and use LET.
       (compiler-macroexpand
         $(#'((,g)
                (append (? (json-object? ,g)
                           (props-klist ,g)
                           ,g)
                        ,(backq .x)))
                        ,(cadr x.))))))

(fn backq (x)
  (?
    (atom x)
      (atomic x)
    (pcase x.
      atom
        $(. ,(enquote x.)
            ,(backq .x))
      quasiquote?
        (qq x)
      quasiquote-splice?
        (qqs x)
      $(. ,(backq x.)
          ,(backq .x)))))

(fn disp (x)
  (pcase x
    quote?
      (enquote-r .x.)
    backquote?
      (backq .x.)
    x))

(fn r (x)
  (? (atom x)
     (disp x)
     (. (r (disp x.))
        (r .x))))

(fn compiler/quote-expand (x)
  (car (r (.. x))))
