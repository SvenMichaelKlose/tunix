(in-package 'c/pa
  '(error stackarg assign))

(fn stackarg (x)
  (!= (get-funinfo .x.)
    (+ (length (!.vars))
       (- (length (!.args))
          (!.arg-pos ..x.)
          1))))

(def-tree-filter assign (x)
  (or (quote? x)
      (%native? x))
    x
  (and (%s? x) ..x)
    $(%s ,(stackarg x))
  (and (%s? x) ..x)
    $(%s
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
  (%slot-value? x)
    $(%slot-value ,(assign .x.) ,..x.))

(var compiler/place-assign assign)

(in-package nil)
