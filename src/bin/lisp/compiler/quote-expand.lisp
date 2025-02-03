(in-package compiler/qex
  '(any-qq? enquote enquote-r
    enqq enqqs backq disp r))

(fn any-quasiquote? (x)
  (or (quasiquote? _)
      (quasiquote-splice? _)))

(fn enquote (x)
  (? (constant-literal? _)
     _
     $(quote ,_)))

(fn enquote-r (x)
  (? (atom _)
     (enquote _)
     $(. ,(enquote-r _.)
         ,(enquote-r ._))))

(fn enqq (x)
  (? (any-quasiquote? (cadr _.))
     $(. ,(backq (cadr _.))
         ,(backq ._))
     $(. ,(cadr _.)
         ,(backq ._))))

(fn enqqs (x)
  (? (any-quasiquote? (cadr _.))
     (error "Illegal ~A as argument to ,@ (QUASIQUOTE-SPLICE)." (cadr _.))
     (with-gensym g
       ; TODO: Make TRANSPILER-MACROEXPAND work and use LET.
       (compiler-macroexpand
         $(#'((,g)
                (append (? (json-object? ,g)
                           (props-klist ,g)
                           ,g)
                        ,(backq ._)))
                        ,(cadr _.))))))

(fn backq (x)
  (?
    (atom _)
      (atomic _)
    (pcase _.
      atom
        $(. ,(enquote _.)
            ,(backq ._))
      quasiquote?
        (qq _)
      quasiquote-splice?
        (qqs _)
      $(. ,(backq _.)
          ,(backq ._)))))

(fn disp (x)
  (pcase _
    quote?
      (enquote-r ._.)
    backquote?
      (backq ._.)
    _))

(fn r (x)
  (? (atom _)
     (disp _)
     (. (r (disp _.))
        (r ._))))

(fn compiler/quote-expand (x)
  (car (r (list x))))
