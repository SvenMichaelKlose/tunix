(load 'compiler/package.lsp)

(fn pass (pin pout f)
  (with-in i (open pin 'r)
    (with-out o (open pout 'w)
      (while (not (eof))
        (awhen (read)
          (setout stdout)
          (print !.)
          (setout o)
          (print (. !. (. .!. (@ f ..!))))
          (reset!))))))

(in-package nil)
