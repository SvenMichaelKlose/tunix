; Line editor
(or (cons? subseq)
    (load "subseq.lisp"))

(fn clrscr ()
  (out 1))
(fn con-col0 ()
  (out 1))
(fn con-clr2eol ()
  (out 1))
(fn con-xy (x y)
  (out 1)
  (out 1))

(var cx 0)

(fn display-line (line)
  (con-col0)
  (@ 'out (subseq line 0 *con-width*))
  (con-clr2eol))

(fn editline (x)
  (with ((line (symbol-name x)))
    (while t
      (display-line line)
      (with ((len (length line))
             (c   (conin)))
        (case c
          +con-left+
            (? (< 0 x)
               (= x (-- x)))
          +con-right+
            (? (< x len)
               (= x (++ x)))
          +con-del+
            (? (< 0 x)
              (= line (append (subseq line 0 (-- x))
                              (subseq line x))))
          +con-enter+
            (list nil (return (symbol line)))
          (con-ctrl? c)
            (list c (return (symbol line)))
          t
            (= line (append (subseq line 0 x)
                            (list c)
                            (subseq line (++ x)))))))))
