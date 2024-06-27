; Replace argument symbols by a pre-allocated set of
; anonymous symbols.  Optionally of inlined functions only,
; so public argument defintions stay readable.

(fn lambda? (x)
  (and (cons? x)
       (car (cons? x))))

(fn ws-expr (tab x)
  (? (atom x)
     (or (cdr (assoc x tab)) x)
     (@ $((x)
           (? (lambda? x)
              (ws-fun ',tab x)
              x))
        (car x))))

(fn ws-body (tab x)
  (@ $((x) (ws-expr ',tab x)) x))

(fn ws-fun (tab f)
  (with (args (car f)
         tab  (+ (pairlis *ws-syms*
                          (+ (butlast args)
                             (ensure-list (last args))))
                 tab)
         xlat $((x)
                 (or (cdr (assoc x ',tab)) x)))
    (cons (@ xlat args)
          (ws-body tab (cdr f)))))
