(load 'compiler/package.lsp)
(require 'let 'prog1 'awhen 'do 'while 'with-in 'with-out 'while)

(fn pass (pin pout f . reset?)
  (with-in i (open pin 'r)
    (with-out o (open pout 'w)
      (while (not (eof))
        (awhen (read)
          (setout stdout)
          (print !.)
          (setout o)
          (? reset?.
             (= *macros* nil))
          (print (. !. (. .!. (@ f ..!))))
          (? reset?.
             (reset!)))))))

(in-package nil)
