(load 'as65/package.lsp)
(require 'when 'progn 'prog1 'do 'dolist 'while 'awhile 'with-in 'with-out)

(fn pass (o addr pathnames first-pass?)
  ; "Single-pass assembly to stream."
  (= *as65-pc* addr)
  (dolist (pn pathnames)
    (when (with-in i (open (print pn) 'r)
            (awhile (asm (read) first-pass?)
              (when o
                (out !))))
      (error "Can't open " pn))))

(fn files (bin-path addr pathnames)
  ; "Assemble a set of files in multiple passes."

  ; First pass, building label database.
  (out '"First pass...")
  (= *as65-labels* nil)
  (pass nil addr pathnames t)
  (terpri)

  ; Secondary passes, until code size settled.
  (out '"Secondary pass...")
  (let (old-end *as65-pc*)
    (while t
      (pass nil addr pathnames nil)
      (when (== old-end *as65-pc*)
        (return nil))
      (= old-end *as65-pc*)))
  (terpri)

  ; Final pass, outputting the binary.
  (message "Assembling to " bin-path '...)
  (with-out o (open bin-path 'w)
    (pass o addr pathnames nil)))

(in-package nil)
