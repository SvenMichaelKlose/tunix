(fn filter-file (pin pout . f)
  ; "Filter expressions from file to file."
  (= f (or f. identity))
  (with-in i (open pin 'r)
    (with-out o (open pout 'w)
      (while (not (eof))
        (print (f (read)))))))
