(fn as65-pass (o addr x)
  ; "Single-pass assembly to stream."
  (= *as65-pc* addr)
  (dolist (pn pathnames)
    (with-in i (open pn 'r)
      (awhile (as65 (read) (& o 2))
        (when o (out !))))))

(fn as65-files (bin-path addr pathnames)
  ; "Assemble a set of files in multiple passes."
  (= *as65-labels* nil)
  (as65-pass nil addr pathnames t)
  (with (old-end *as65-pc*)
    (with-out o (open bin-path 'w)
      (as65-pass o addr pathnames t))))
