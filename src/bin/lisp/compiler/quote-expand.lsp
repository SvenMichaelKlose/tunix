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

(def-tree-filter enquote-r (x)
  (atom x) (enquote x))

(fn enqq (x)
  $(. ,(? (any-quasiquote? (cadr x.))
          (backq (cadr x.))
          (cadr x.))
      ,(backq .x)))

(fn enqqs (x)
  (? (any-quasiquote? (cadr x.))
     (error "Illegal argument to ,@: "
            (cadr x.))
     (let (g (symbol))
       (compiler-macroexpand
         $(#'((,g)
                (append
                    (? (json-object? ,g)
                       (props-klist ,g)
                       ,g)
                    ,(backq .x)))
              ,(cadr x.))))))

(def-tree-filter backq (x)
  (atom x)
    (atomic x)
  (pcase x.
    atom $(. ,(enquote x.)
             ,(backq .x))
    quasiquote?        (qq x)
    quasiquote-splice? (qqs x)))

(fn disp (x)
  (pcase x
    quote?     (enquote-r .x.)
    backquote? (backq .x.)
    x))

(def-tree-filter r (x)
  (atom x) (disp x))

(fn compiler/quote-expand (x)
  (car (r (.. x))))
