(in-package 'compiler/place-assign
  '(error stackarg assign))

(fn stackarg (x)
  (let fi (get-funinfo .x.)
    (+ (length (fi.vars))
       (- (length (fi.args))
          (fi.arg-pos ..x.)
          1))))

(define-tree-filter assign (x)
  (or (quote? x)
      (%native? x))
    x
  (and (%stackarg? x) ..x)
    $(%stack
       ,(stackarg x))
  (and (%stack? x) ..x)
    $(%stack
       ,(or ((get-funinfo .x.).var-pos ..x.)
            (stackarg x)))
  (and (%vec? x) ...x)
    $(%vec
       ,(assign .x.)
       ,((get-funinfo ..x.).scoped-var-index ...x.))
  (and (%set-vec? x) ....x)
    $(%set-vec
       ,(assign .x.)
       ,((get-funinfo ..x.).scoped-var-index ...x.)
       ,(assign ....x.))
  (named-lambda? x)
    (copy-lambda x :body (assign (lambda-body x)))
  (%slot-value? x)
    $(%slot-value ,(assign .x.) ,..x.))

(fn place-assign (x)
  (assign x))

(in-package nil)
