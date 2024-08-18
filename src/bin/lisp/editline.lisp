(or (cons? subseq)
    (load "subseq.lisp"))

(var *con-width* 22)
(var *con-height* 23)

;;; State

(var saved? nil)
(var lx 0)      ; Line X position.
(var ln 0)
(var conln 0)   ; First ln on console.

;;; Console basics

(fn con-cr ()
  (out 13)   ; CR+LF
  (out 145)) ; UP

(fn con-clr2eol ()
  (dotimes ((- *con-width* cx))
    (out ' ')))

(fn con-xy (x y)
  (out 1)
  (out 1))

(fn update-line (line)
  (con-cr)
  (@ out (subseq line 0 *con-width*))
  (= cx (length line))
  (con-clr2eol))

; Update screen from ln line on.
; Scroll if necessary.
(fn display ())

(fn status (msg))

;;; Editing

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
            (when (< 0 cx)
              (= saved? nil)
              (= line (append (subseq line 0 (-- cx))
                              (subseq line cx))))
          (< c \ )
            (progn
              (conputback)
              (return (symbol line)))
          t
            (progn
              (= saved? nil)
              (= line (append (subseq line 0 cx)
                              (list c)
                              (subseq line (++ cx))))))))))

(var lines nil)

(fn read-lines (x)
  (with-input (open x 'r)
    (with-queue q
      (while (not (eof))
        (enqueue q (read-line))))))

(fn save-file ()
  (put-file x lines)
  (? (err)
     (status "Cannot save.")
     (= saved? t)))

(fn del-line (ln)
  (= file (append (subseq lines 0 (-- ln))
                  (subseq lines (++ ln)))))

(fn choose x
  (while t
    (!= (conin)
      (and (member ! x)
           (return !)))))

(fn quit-editor ()
  (or modified?
      (return t))
  (status "Save first (y/n/c)?")
  (!= (choose \y \n \c)
    (and (== ! \y)
         (save-file))
    (not (== ! \c))))

(fn edit-lines ()
  (while t
    (status fname)
    (display)
    (let lcons (nth file ln)
      (!? (edit-line (car lcons))
          (setcar lcons newl)
          (del-line ln)))
    (case (conin)
      +arr-up+
        (? (< 0 ln)
           (= ln (-- ln)))
      +arr-down+
        (? (< ln (length file))
           (= ln (++ ln)))
      +hotkey+
        (case (conin)
          \s (save-file)
          \q (and (quit-editor)
                  (return))

      t
        (conputback))))

(fn edit (file)
  (= lines (read-lines file))
  (= saved? t)
  (edit-lines)
  (out "Bye!")(terpri))
