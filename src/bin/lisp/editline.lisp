; Line editor
(or (cons? subseq)
    (load "subseq.lisp"))

(var *con-width* 22)

(fn con-cr ()
  (out 13)   ; CR+LF
  (out 145)) ; UP

(fn con-clr2eol ()
  (dotimes ((- *con-width* cx))
    (out ' ')))

(fn con-xy (x y)
  (out 1)
  (out 1))

(var cx 0)

(fn display-line (line)
  (con-cr)
  (@ out (subseq line 0 *con-width*))
  (= cx (length line))
  (con-clr2eol))

(fn editline (x)
  (with ((line (symbol-name x)))
    (while t
      (display-line line)
      (with ((len (length line))
             (c   (conin)))
        (case c
          +arr-left+
            (? (< 0 cx)
               (= cx (-- cx)))
          +arr-right+
            (? (< cx len)
               (= cx (++ cx)))
          +del+
            (? (< 0 cx)
              (= line (append (subseq line 0 (-- cx))
                              (subseq line cx))))
          (< c \ )
            (progn
              (or (== c +enter+)
                  (conputback))
              (return (symbol line)))
          t
            (= line (append (subseq line 0 cx)
                            (list c)
                            (subseq line (++ cx)))))))))
