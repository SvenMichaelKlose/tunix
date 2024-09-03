(@ '((x)
      (or (load (symbol (append (symbol-name x)
                                (symbol-name ".lisp"))))
          (error "Can't load " x ".lisp")))
   '("prog1" "when" "unless" "dolist" "alet" "aif" "while"
     "awhile" "queue" "with-queue" "dotimes" "incdec"
     "nth" "awhen" "mapcar" "mapcan" "case" "group"
     "cbm-keycode" "cbm-con" "ls" "cut-at"
     "with-in" "with-out"))

(var +hotkey+ 11)   ; CBM Ctrl-K

;;; State

(var *filename* "edit-help.md")
(var *lines* nil)
(var *saved?* nil)
(var *err* nil)
(var *ox* 0)      ; Line X offset.
(var *lx* 0)      ; Line X position, relative to *OX*.
(var *ln* 0)
(var *conln* 0)   ; # of first displayed line.

;;; Terminal control

(fn clrscr ()
  (out 12))

(fn con-xy (x y)
  (out 1)
  (out x)
  (out y))

(fn con-clrset (x f)
  (out (? x 3 2))
  (out f))

(fn con-crs (x)
  (con-clrset x 1))

(fn con-rvs (x)
  (con-clrset x 2))

(fn con-direct (x)
  (con-clrset x 4))

(fn con-get (x)
  (out x)
  (-- (conin)))

(fn con-x ()
  (con-get 4))

(fn con-y ()
  (con-get 5))

;;; Display

(var *spaces* nil)
(dotimes (i *con-w*)
  (push \  *spaces*))

(fn line-len (x)
  (? (list? x)
     (length x)
     (slength x)))

(fn update-line (l y)
  (con-xy (or *ox* 0) y)
  (when l
    (outlim *con-w*)
    (out l))
  (out (or (nthcdr (line-len l) *spaces*)
           "")))

(fn update-screen ()
  (with ((y 0)
         (l (nthcdr *conln* *lines*)))
    (dotimes (i (-- *con-h*))
      (update-line (and l (car l)) y)
      (!++ y)
      (= l (cdr l)))))

(fn clr-status ()
  (con-xy 0 (-- *con-h*))
  (out *spaces*))

(fn prompt (msg)
  (clr-status)
  (con-xy 0 (-- *con-h*))
  (out msg))

(fn prompt-in (msg l)
  (prompt msg)
  (= *ox*  (con-x))
  (prog1 (edit-line l (-- *con-h*))
    (conin)
    (clr-status)
    (= *ox* nil)))

(fn status-pos ()
  (con-rvs t)
  (con-xy (- *con-w* 8) (-- *con-h*))
  (print (++ *lx*))(out ",")(print (++ *ln*))
  (con-rvs nil)
  (out "    "))

(fn status ()
  (con-xy 0 (-- *con-h*))
  (con-rvs t)
;  (funcall out (or *filename* "")
;               (? (not *lines*)
;                  " (new)"
;                  "")
;               (or *err* ""))
  (status-pos))

;;; Line editing

(fn del-char (x)
  (= *saved?* nil)
  (= line (nconc (subseq line 0 x)
                 (subseq line (++ x)))))

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (l y)
  (con-crs t)
  (let line (? l (symbol-name l) nil)
    ; Don't have cursor past line end.
    (let n (length line)
      (and (> *lx* n)
           (= *lx* (? (> n 0) (-- n) 0))))
    (while (not (eof))
      (update-line line y)
      (con-xy (+ (or *ox* 0) *lx*) y)
      (with ((len (length line))
             (c   (while (not (eof))
                    (awhen (conin)
                      (return !)))))
        (case c
          +arr-left+
            (? (< 0 *lx*)
               (!-- *lx*))
          +arr-right+
            (? (< *lx* len)
               (!++ *lx*))
          +bs+
            (progn
              (when (== 0 *lx*)
                (putback)
                (return (symbol line)))
              (? (< 0 *lx*)
                 (del-char (!-- *lx*))))
          (progn
            ; Put back unknown key and return line.
            (when (or (< c \ ) (> c 126))
              (putback)
              (return (symbol line)))
            ; Insert char and step right.
            (= *saved?* nil)
            (= line (? (== 0 *lx*)
                       (nconc (list c) line)
                       (!= (cut-at *lx* line)
                         (nconc line (list c) !))))
            (!++ *lx*)))))))

;;; File I/O

(fn read-lines ()
  (with-queue q
    (while (not (eof))
      (enqueue q (read-line)))))

(fn save-file ()
  (with-out o (open (prompt-in "Save: " *filename*) 'w)
    (unless (err)
      (prompt "...")
      (dolist (l *lines*)
        (out l)
        (terpri))))
  (? (err)
     (and (status "Cannot save.") nil)
     (= *saved?* t)))

(fn load-file ()
  (with-in i (open (prompt-in "Load: " *filename*) 'r)
    (? (or (not i) (err))
       (progn
         (= *err* (symbol (nconc (symbol-name "No ")
                                 (symbol-name f))))
         (return nil))
       (progn
         (prompt "...")
         (= *lines* nil)
         (= *lines* (read-lines)))))
  (and (err)
       (status "Load error.") nil))


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

;;; Text editing

(fn del-line ()
  (= *lines* (? (== 0 *lx*)
                (cdr *lines*)
                (!= (cut-at *lx* *lines*)
                  (nconc *lines* (cdr !))))))

(fn join-line ()
  (let prev (nth (-- *ln*) *lines*)
    (with ((prev-len (slength prev))
           (joined   (list (symbol (nconc (symbol-name prev)
                                          (symbol-name (nth *ln* *lines*)))))))
      (= *lines* (? (== 1 *ln*)
                    (nconc joined (cddr *lines*))
                    (!= (cut-at (-- *ln*) *lines*)
                      (nconc *lines* joined (cddr !)))))
      (!-- *ln*)
      (= *lx* prev-len))))

(fn split-line (l x)
  (? (== 0 x)
     (list ""
           l)
     (let lc (symbol-name l)
       (!= (cut-at x lc)
         (list (symbol lc)
               (symbol !))))))

(fn ins-line ()
  (let l (split-line (nth *ln* *lines*) *lx*)
    (= *lines* (? (== 0 *ln*)
                  (nconc l (cdr *lines*))
                  (!= (cut-at *ln* *lines*)
                    (nconc *lines* l (cdr !))))))
  (!++ *ln*)
  (= *lx* 0))

(fn editor-cmds ()
  (prompt "Ctrl+K+")
  (case (conin)
    \l  (load-file)
    \s  (save-file)
    \q  (and (quit-editor)
             (return t))
    \e  (progn
          (prompt ": ")
          (con-direct nil)
          (!= (eval (read))
            (terpri)
            (print !))
          (conin)
          (con-direct t))))

; Navigate up and down lines, catch commands.
(fn edit-lines ()
  (while (not (eof))
    (update-screen)
    (status)
    no-screen-update
    (status-pos)
    (let line (nthcdr *ln* *lines*)
      (!= (edit-line (car line) (- *ln* *conln*))
        (setcar line !)))
    (case (conin)
      +enter+
        (ins-line)
      +bs+
        (? (< 0 *ln*)
           (join-line))
      +arr-up+
        (progn
          (when (< 0 *ln*)
            (!-- *ln*))
          (go no-screen-update))
      +arr-down+
        (progn
          (when (< *ln* (-- (length *lines*)))
            (!++ *ln*))
          (go no-screen-update))
      +hotkey+
        (? (editor-cmds)
           (return nil)))))

(fn edit file
  (= *lx* 0)
  (= *ln* 0)
  (= *conln* 0)
  (= saved? t)
  (clrscr)
  (con-direct t)
  ;(load-file)
  (= *lines* (list ""))
  (edit-lines)
  (clrscr)
  (con-direct nil)
  *filename*)
