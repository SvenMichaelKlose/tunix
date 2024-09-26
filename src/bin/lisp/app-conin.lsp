(require 'while 'do 'let 'with 'progn)

(while (not (eof))
  (awhen (conin)
    (print !)
    (and (< ! \ )
         (return nil))))
