(var +hotkey+ 11)   ; CBM Ctrl-K

;;; State

(var *lines* nil)
(var *saved?* nil)
(var lx 0)      ; Line X position.
(var ln 0)
(var conln 0)   ; # of first displayed line.

;;; Console basics

(fn clrscr (x y))
(fn con-xy (x y))
(fn con-crs (onoff))
(fn con-rvs (onoff))

;;; Rendering

(fn update-line (l y)
  (out (subseq l 0 *con-w*))
  (dotimes (i (- *con-w* (length l)))
    (out \ )))

(fn update-screen ()
  (con-home)
  (let y 0
    (dolist (l (subseq *lines* con-ofs (-- *con-h*)))
      (update-line l y)
      (!++ y))))

(fn status msg
  (con-xy 0 *con-h*)
  (@ out msg))

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
          +del+
            (? (< 0 lx)
              (del-char lx))
          +bs+
            (? (< 0 lx)
              (del-char (!-- lx)))
          (< c \ )
            (progn
              (putback)
              (return (symbol line)))
          (progn
            (= *saved?* nil)
            (= line (append (subseq line 0 lx)
                            (list c)
                            (subseq line lx)))
            ))))
            ;(!++ lx)))))
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
    (let lcons (nth *lines* ln)
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
  (messages "Cleaning up, please wait...")
  (gc)
  ;(= *lines* (read-lines file))
  (= saved? t)
  (edit-lines)
  (status "Bye!")(con-crson)(terpri))

(gc)

(message "Editing line...")
(edit-line "TUNIX")
