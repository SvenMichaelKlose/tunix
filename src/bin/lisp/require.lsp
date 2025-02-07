(fn require x
  (block nil
    loop
    (or x (return nil))
    (((i)
       (or (macro? i)
           (builtin? i)
           (cons? (symbol-value i))
           (or (load (symbol (nconc (symbol-name i)
                                    (symbol-name '.lsp))))
               (error i))))
     (car x))
    (= x (cdr x))
    (go loop)))
