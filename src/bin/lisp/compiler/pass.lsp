(require 'let 'prog1 'awhen 'do 'while
         'with-in 'with-out 'while)

(fn compiler/pass (pin pout f . reset?)
  (with-in i (open pin 'r)
    (with-out o (open pout 'w)
      (while (not (eof))
        (awhen (read)
          (setout stdout)
          (print !.)
          (setout o)
          (print (. !.
                    (. .!.
                          (@ f ..!))))
          (? reset?.
             (reset!)))))))
