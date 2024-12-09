(var *all-mnem65* (append (apply append *mn-6502*)
                          (remove t (remove nil (apply append (remove t (remove nil (apply append *6502*))))))))

(fn labeldef? (x)
  (!= (slength x)
    (and (< 1 !)
         (== \: (char-at x (-- !))))))

(fn mnem? (x)
  (member x *all-mnem65*))

(or (mnem? 'lda)
    (error "MNEM? not working"))

(fn as65-parse (x)
  (block nil
    (awhen x.
      (unless (symbol? !)
        (error "Symbol expected"))

      (when (labeldef? !)
        (return (. .x (. 'label !))))
      (and (symbol? !)
           (eq ': .x.)
           (return (. ..x (. 'label !))))

      (or (mnem? !)
          (error "Menomic expected"))
      (let (inst nil)
        (acons! 'mnem ! inst)
        (= x .x)

        (awhen x.
          (when (cons? !)   ; (a) / (a,x) / expr
            (and (== 2 (length !)) ; Lisp expression
                 (cons? (cadr !)) ; (a,x) -> (a (quote x))
                 (eq 'unquote (car .!.))
                 (eq 'x (cadr .!.))
                 (return (. .x
                            (. (. 'mode 'indx)
                               (. (. 'op !.)
                                  inst)))))
            (acons! 'mode 'ind inst)
            (acons! 'op ! inst))
          (unless (cons? !)
            (acons! 'op ! inst))
          (= x .x)
          (awhen x.
            (or (eq 'unquote (car !))
                (error "QUOTE or eol"))
            (!= .!.
              (or (eq 'x !)
                  (eq 'y !)
                  (error ",x or ,x!"))
              (acons! 'absr ! inst))
            (= x .x)))
        (. x inst)))))
