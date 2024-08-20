(or (cons? while)
    (load "while.lisp"))
(or (cons? subseq)
    (load "subseq.lisp"))
(or (cons? case)
    (load "case.lisp"))

(var *con-width* 22)
(var *con-height* 23)
(var +array-up+ 23)
(var +array-down+ 23)
(var +array-left+ 23)
(var +array-right+ 23)
(var +bs+ 23)
(var +hotkey+ 23)   ; Ctrl-K?

;;; State

(var *lines* nil)
(var *saved?* nil)
(var lx 0)      ; Line X position.
(var ln 0)
(var conln 0)   ; First ln on console.

;;; Console basics

(fn con-cr ()
  (out 13)   ; CR+LF
  (out 145)) ; UP

(fn con-clr2eol ()
  (dotimes ((- *con-width* cx))
    (out \ )))

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

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (x)
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
          +bs+
            (when (< 0 cx)
              (= *saved?* nil)
              (= line (append (subseq line 0 (-- cx))
                              (subseq line cx))))
          (< c \ )
            (progn
              (putback)
              (return (symbol line)))
          (progn
            (= *saved?* nil)
            (= line (append (subseq line 0 cx)
                            (list c)
                            (subseq line (++ cx))))))))))

(var lines nil)

(fn skipctrls ()
  (while (not (eof))
    (= (conin))
    (? (< c \ )
       (return nil))))

(fn read-line ()
  (symbol
    (with-queue q
      (let c nil
        (skipctrls)
        (? (eof)
           (return nil))
        (while (not (eof))
          (= c (conin))
          (? (< c \ )
             (return nil))
          (enqueue q))))))

(fn read-lines (x)
  (with-input (open x 'r)
    (with-queue q
      (while (not (eof))
        (enqueue q (read-line))))))

(fn save-file ()
  (with-output (open pathname 'w)
    (@ out *lines*))
  (? (err)
     (status "Cannot save.")
     (= *saved?* t)))

(fn del-line (ln)
  (= *lines* (append (subseq lines 0 (-- ln))
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

; Navigate up and down lines, catch commands.
(fn edit-lines ()
  (status *filename* (? (not *lines*) " (new)"))
  (while t
    ; Update screen.
    (display)

    ; Edit current line.
    (let lcons (nth *lines* ln)
      (!? (edit-line (car lcons))
          ; Replace line in list.
          (setcar lcons !)
          ; Remove line.
          (del-line ln)))

    (status *filename*)

    ; Handle line motion and commands.
    (case (conin)
      +arr-up+
        (? (< 0 ln)
           (= ln (-- ln)))
      +arr-down+
        (? (< ln (length *lines*))
           (= ln (++ ln)))
      +hotkey+
        (case (conin)
          \s (save-file)
          \q (and (quit-editor)
                  (return)))
      (putback))))

(fn edit (file)
  (= *lines* (read-lines file))
  (= saved? t)
  (edit-lines)
  (out "Bye!")(terpri))
