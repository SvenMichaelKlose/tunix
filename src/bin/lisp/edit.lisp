(var +hotkey+ 11)   ; CBM Ctrl-K

;;; State

(var *filename* "code.lisp")
(var *lines* nil)
(var *saved?* nil)
(var lx 0)      ; Line X position.
(var ln 0)
(var conln 0)   ; # of first displayed line.

;;; Console basics
;;; Super-minimalistic terminal support with only
;;; CLRSCR, HOME, CR and LF.

(fn clrscr ()
  (out 12))

(fn con-xy (x y)
  (out 1)
  (out x)
  (out y))

(fn con-crs (x)
  (out (? x 3 2))   ; CLR/SET
  (out 1))          ; cursor

(fn con-rvs (onoff))

;;; Rendering

; Update line up to a particular number of chars.
(fn update-line (l y)
  (con-xy 0 y)
  (out (or (subseq l 0 *con-w*) ""))
  (dotimes (i (- *con-w* (length l)))
    (out \ ))
  (con-xy cx y))

(fn update-screen ()
  (con-xy 0 0)
  (let y 0
    (dolist (l (subseq *lines* conln *con-h*))
      (update-line (symbol-name l) y)
      (!++ y))))

(fn status msg
  (con-xy 0 (-- *con-h*))
  (con-rvs t)
  (apply out msg)
  (con-rvs nil))

;;; Editing

(fn del-char (x)
  (= *saved?* nil)
  (= line (append (subseq line 0 (-- x))
                  (subseq line x))))

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (x)
  (let line (? x (symbol-name x) nil)
    (while (not (eof))
      (update-line line 0)
      (con-xy lx (- ln conln))
      (con-crs t)
      (with ((len (length line))
             (c   (while (not (eof))
                    (awhen (conin)
                      (return !)))))
        (case c
          +arr-left+
            (? (< 0 lx)
               (!-- lx))
          +arr-right+
            (? (< lx len)
               (!++ lx))
          +bs+
            (? (<= 0 lx)
               (del-char (!-- lx)))
          +del+
            (? (<= 0 lx)
               (del-char lx))
          (progn
            (when (< c \ )
              (putback)
              (return (symbol line)))
            (= *saved?* nil)
            (= line (append (subseq line 0 lx)
                            (list c)
                            (subseq line lx)))
            (!++ lx)))))
    (con-crs nil)))

(var lines nil)

(fn skipctrls ()
  (while (not (eof))
    (when (<= \  (conin))
      (putback)
      (return))
    t))

(fn read-line ()
  (symbol
    (with-queue q
      (let c nil
        (? (skipctrls)
           (return))
        (while (not (eof))
          (= c (conin))
          (? (< c \ )
             (return))
          (enqueue q c))))))

(fn read-lines (x)
  (with-input i (open x 'r)
    (with-queue q
      (while (not (eof))
        (enqueue q (read-line))))))

(fn save-file ()
  (with-output o (open pathname 'w)
    (@ out *lines*))
  (? (err)
     (and (status "Cannot save.") nil)
     (= *saved?* t)))

(fn del-line (ln)
  (= *lines* (append (subseq lines 0 (-- ln))
                     (subseq lines (++ ln)))))

(fn choose x
  (while (not (eof))
    (!= (conin)
      (and (member ! x)
           (return !)))))

(fn quit-editor ()
  (or *saved?*
      (return t))
  (status "Save first (y/n/c)?")
  (!= (choose \y \n \c)
    (and (== ! \y)
         (save-file))
    (not (== ! \c))))

; Navigate up and down lines, catch commands.
(fn edit-lines ()
  (status *filename* (? (not *lines*) " (new)"))
  (while (not (eof))
    (update-screen)

    ; Edit current line.
    (let lcons (nthcdr ln *lines*)
      (!? (edit-line (car lcons))
          (setcar lcons !) ; Replace line in list.
          (del-line ln))) ; Remove line.

    (status *filename*)

    ; Handle line motion and commands.
    (case (conin)
      +arr-up+
        (? (< 0 ln)
           (!-- ln))
      +arr-down+
        (? (< ln (length *lines*))
           (!++ ln))
      +hotkey+
        (case (conin)
          \s (save-file)
          \q (and (quit-editor)
                  (return)))
      (putback))))

(fn edit file
  ;(= *lines* (read-lines file))
  (= *lines* (list "TUNIX Lisp IDE"))
  (= saved? t)
  (clrscr)
  (edit-lines)
  (status "Bye!")(con-crson)(terpri))

(gc)
