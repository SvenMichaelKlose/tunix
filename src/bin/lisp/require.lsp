(or (macro? 'dolist)
    (load "dolist.lsp"))

(fn require x
  (dolist (i x)
    (or (macro? i)
        (builtin? i)
        (cons? (symbol-value i))
        (or (load (symbol (nconc (symbol-name i)
                                 (symbol-name ".lsp"))))
            (error i)))))
