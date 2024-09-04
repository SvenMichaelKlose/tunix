(@ '((x)
      (or (load (symbol (append (symbol-name x)
                                (symbol-name ".lisp"))))
          (error "Can't load " x ".lisp")))
   '("prog1" "when" "unless" "dolist" "alet" "aif" "while"
     "awhile" "queue" "with-queue" "dotimes" "incdec"
     "nth" "awhen" "mapcar" "mapcan" "case" "group"
     "cbm-keycode" "cbm-con" "cut-at"
     "with-in" "with-out" "with-global"))

;;; State

(var *filename* "edit-help.md")
(var *lines* nil)
(var *saved?* nil)  ; Modifications saved?
(var *mod?* nil)    ; Line modified?
(var *oline* nil)   ; Original line.
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
  (con-xy *ox* y)
  (when l
    (outlim *con-w*)
    (out l))
  (!? (nthcdr (line-len l) *spaces*)
      (out !)))

(fn update-screen ()
  (with ((y 0)
         (l (nthcdr *conln* *lines*)))
    (dotimes (i (-- *con-h*))
      (update-line (and l (car l)) y)
      (!++ y)
      (= l (cdr l)))))

(fn outrvs msg
  (con-rvs t)
  (out msg)
  (con-rvs nil))

(fn clr-status ()
  (con-xy 0 (-- *con-h*))
  (outrvs *spaces*))

(fn prompt (msg)
  (con-direct t)
  (clr-status)
  (con-xy 0 (-- *con-h*))
  (outrvs msg))

(fn prompt-in (msg l)
  (prompt msg)
  (with-global *ox* (slength msg)
    (with-global *lx* (slength l)
      (prog1
        (while t
          (!= (edit-line l (-- *con-h*))
            (and (== (conin) +enter+)
                 (return !))
            (= l !)))
        (clr-status)))))

(fn status-pos ()
  (con-rvs t)
  (con-xy (- *con-w* 8) (-- *con-h*))
  (print (++ *lx*))(out ",")(print (++ *ln*))
  (out " ")
  (con-rvs nil))

(fn status-msg ()
  (con-xy 0 (-- *con-h*))
  (outrvs (or *filename* "")
          (? (not *lines*)
             " (new)"
             "")
          (or *err* "")))

;;; Line editing

(fn del-char (x)
  (= *saved?* nil)
  (= line (nconc (subseq line 0 x)
                 (subseq line (++ x)))))

(fn go-eol (line)
  (let n (length line)
    (= *lx* (? (> n 0) n 0))))

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (l y)
  (con-crs t)
  (= *mod?* nil)
  (= *oline* l)
  (let line (? l (symbol-name l) nil)
    ; Don't have cursor past line end.
    (and (> *lx* (length line))
         (go-eol line))
    (while (not (eof))
      (update-line line y)
      (con-xy (+ (or *ox* 0) *lx*) y)
      (with ((len  (length line))
             (c    (while (not (eof))
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
          1 ; Ctrl-A
            (= *lx* 0)
          4 ; Ctrl-D
            (progn
              (= *mod?* t)
              (= line nil)
              (= *lx* 0))
          5 ; Ctrl-E
            (go-eol line)
          (progn
            ; Put back unknown key and return line.
            (when (or (< c \ ) (> c 126))
              (putback)
              (return (? *mod?* (symbol line) *oline*)))
            ; Insert char and step right.
            (= *mod?* t)
            (= *saved?* nil)
            (= line (? (== 0 *lx*)
                       (nconc (list c) line)
                       (!= (cut-at *lx* line)
                         (nconc line (list c) !))))
            (!++ *lx*)))))))

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

;;; File I/O

(fn read-lines ()
  (with-queue q
    (while (not (eof))
      (enqueue q (read-line)))))

(fn save-file ()
  (prompt "...")
  (? (with-out o (open (prompt-in "Save: " *filename*) 'w)
       (unless (err)
         (dolist (l *lines*)
           (out l)
           (terpri))))
     (and (= *err* "Cannot save.") nil)
     (progn
       (clr-status)
       (= *saved?* t))))

(fn load-file ()
  (let f (prompt-in "Load: " *filename*)
    (? (with-in i (open f 'r)
         (prompt "...")
         (= *filename* f)
         (= *lines* nil)
         (= *lines* (read-lines))
         (clr-status))
       (= *err* "Cannot load.") nil)))

(fn choose x
  (while (not (eof))
    (!= (conin)
      (and (member ! x)
           (return !)))))

(fn quit-editor ()
  (and *saved?*
       (return 'quit))
  (prompt "Save (y/n/c)?")
  (!= (choose \y \n \c)
    (clr-status)
    (and (== ! \y)
         (save-file)
         'quit)
    (or (== ! \c)
        'quit)))

(fn prompt-ok ()
  (prompt-in "Hit ENTER:" ""))

(fn editor-cmds ()
  (prompt "Ctrl+K+")
  (case (conin)
    \l  (load-file)
    \s  (save-file)
    \r  (when (save-file)
          (clrscr)
          (con-direct nil)
          (load *filename*)
          (prompt-ok))
    \q  (quit-editor)
    \e  (progn
          (prompt "Eval: ")
          (con-direct nil)
          (!= (eval (read))
            (terpri)
            (print !)
            (terpri))
          (prompt-ok))))

; Navigate up and down lines, catch commands.
(fn edit-lines ()
  (con-direct t)
  (while (not (eof))
    (update-screen)
    (status-msg)
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
        (when (< 0 *ln*)
          (!-- *ln*)
          (? (< (- *ln* *conln*) 0)
             (!-- *conln*)
             (go no-screen-update)))
      +arr-down+
        (when (< *ln* (-- (length *lines*)))
          (!++ *ln*)
          (? (>= (- *ln* *conln*) (-- *con-h*))
             (!++ *conln*)
             (go no-screen-update)))
      12 ; Ctrl-L
        nil ; Redraw screen
      11 ; Ctrl+K
        (and (eq 'quit (editor-cmds))
             (return nil)))))

(fn edit file
  (= *lx* 0)
  (= *ln* 0)
  (= *conln* 0)
  (= saved? t)
  (clrscr)
  (?
    file
      (progn
        (= *filename* (car file))
        (load-file))
    (and (not *lines*)
         *filename*)
      (load-file))
  (edit-lines)
  (clrscr)
  (con-direct nil)
  *filename*)
