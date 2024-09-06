(@ '((x)
      (or (load (symbol (append (symbol-name x)
                                (symbol-name ".lisp"))))
          (error "Can't load " x ".lisp")))
   '("let" "do" "prog1" "progn" "when" "unless" "dolist" "alet" "aif" "while"
     "awhile" "queue" "with-queue" "dotimes" "incdec"
     "nth" "awhen" "mapcar" "mapcan" "case" "group"
     "cbm-keycode" "cbm-con" "con" "cut-at"
     "with-in" "with-out" "with-global"))

;;; State

(var *filename* "edit-help.md")
(var *lines* nil)
(var *saved?* nil)  ; Modifications saved?
(var *line* nil)
(var *oline* nil)   ; Original line.
(var *mod?* nil)    ; Line modified?
(var *err* nil)
(var *ln* 0)
(var *conln* 0) ; # of first displayed line.
(var *ox* 0)    ; Line X offset.
(var *lx* 0)    ; Line X position, relative to *OX*.
(var *old-conln* 0)
(var *old-ln* 0)

(fn reset-ln ()
  (= *lx* 0)
  (= *ln* 0)
  (= *conln* 0)
  (= *old-conln* -1)
  (= *old-ln* -1)
  (= *update-ln* nil)
  (= *saved?* t))

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

(fn print-lines (s)
  (with ((y s)
         (l (nthcdr (+ *conln* s) *lines*)))
    (dotimes (i (- (-- *con-h*) s))
      (update-line (and l (car l)) y)
      (!++ y)
      (= l (cdr l)))))

(fn update-screen? ()
  ; Adjust *conln* to keep cursor position visible.
  (? (< (- *ln* *conln*) 0)
     (= *conln* 0))
  (? (>= (- *ln* *conln*) (-- *con-h*))
     (= *conln* (- *ln* (- *con-h* 2))))
  (not (== *old-conln* *conln*)))

(fn update-screen ()
  (?
    *update-ln*
      (print-lines *update-ln*)
    (update-screen?)
      (print-lines 0))
  (= *old-conln* *conln*)
  (= *old-ln* *ln*)
  (= *update-ln* nil))

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

(fn prompt-ok ()
  (prompt-in "Hit ENTER:" ""))

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
  (= *line* (nconc (subseq *line* 0 x)
                   (subseq *line* (++ x)))))

(fn go-eol ()
  (let n (length *line*)
    (= *lx* (? (> n 0) n 0))))

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (l y)
  (con-crs t)
  (= *mod?* nil)
  (= *oline* l)
  (let *line* (? l (symbol-name l) nil)
    ; Don't have cursor past line end.
    (and (> *lx* (length *line*))
         (go-eol))
    (while (not (eof))
      (update-line *line* y)
      (con-xy (+ (or *ox* 0) *lx*) y)
      (with ((len  (length *line*))
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
                (return (symbol *line*)))
              (? (< 0 *lx*)
                 (del-char (!-- *lx*))))
          1 ; Ctrl-A
            (= *lx* 0)
          4 ; Ctrl-D
            (progn
              (= *mod?* t)
              (= *line* nil)
              (= *lx* 0))
          5 ; Ctrl-E
            (go-eol)
          (progn
            ; Put back unknown key and return line.
            (when (or (< c \ ) (> c 126))
              (putback)
              (return (? *mod?* (symbol *line*) *oline*)))
            ; Insert char and step right.
            (= *mod?* t)
            (= *saved?* nil)
            (= *line* (? (== 0 *lx*)
                         (nconc (list c) *line*)
                         (!= (cut-at *lx* *line*)
                           (nconc *line* (list c) !))))
            (!++ *lx*)))))))

;;; Text editing

;(fn del-line ()
;  (= *update-ln* *ln*)
;  (= *lines* (? (== 0 *ln*)
;                (cdr *lines*)
;                (!= (cut-at *ln* *lines*)
;                  (nconc *lines* (cdr !))))))

(fn join-line ()
  (= *update-ln* *ln*)
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
  (= *update-ln* *ln*)
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

(fn list-dir ()
  (awhen (and (builtin? opendir)
              (opendir))
    (clrscr)
    (con-direct nil)
    (awhile (readdir !)
      (print (car !)))
    (closedir !)
    (terpri)
    (con-direct t)))

(fn save-file f
  (let uf (? (not f)
             (progn
               ;(list-dir)
               (prompt-in "Save: " *filename*)))
    (prompt "...")
    (? (with-out o (open (or (car f) uf) 'w)
         (? uf
            (= *filename* uf))
         (dolist (l *lines*)
           (out l)
           (terpri)))
       (and (= *err* "Cannot save.") nil)
       (progn
         (clr-status)
         (= *saved?* t)))))

(fn load-file ()
  ;(list-dir)
  (let f (prompt-in "Load: " *filename*)
    (? (with-in i (open f 'r)
         (prompt "...")
         (= *filename* f)
         (= *lines* nil)
         (= *lines* (read-lines))
         (reset-ln)
         (clr-status))
       (= *err* "Cannot load.") nil)))

(fn choose x
  (while (not (eof))
    (!= (conin)
      (and (member ! x)
           (return !)))))

(fn quit-editor ()
  (block nil
    (and *saved?*
         (return 'quit))
    (prompt "Save (y/n/c)?")
    (!= (choose \y \n \c)
      (clr-status)
      (and (== ! \y)
           (save-file)
           'quit)
      (or (== ! \c)
          'quit))))

(fn editor-cmds ()
  (prompt "Ctrl+K: ")
  (case (conin)
    \l  (load-file)
    \s  (save-file)
    \d  (progn
          (list-dir)
          (prompt-ok))
    \r  (progn
          (save-file "_ctrlkr.tmp")
          (= *old-conln* -1)
          (clrscr)
          (con-direct nil)
          (load "_ctrlkr.tmp")
          (prompt-ok))
    \q  (quit-editor)
    \e  (progn
          (prompt "Eval: ")
          (= *old-conln* -1)
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
        ; Check if for screen update range above.
        (when (< 0 *ln*)
          (!-- *ln*))
      +arr-down+
        (when (< *ln* (-- (length *lines*)))
          (!++ *ln*))
      11 ; Ctrl+K: Command
        (and (eq 'quit (editor-cmds))
             (return nil))
      12 ; Ctrl-L: Redraw screen
        (clrscr)
      14 ; Ctrl-N: Page down
        (progn
          (= *ln* (+ *ln* (-- *con-h*)))
          (!= (-- (length *lines*))
            (? (>= *ln* !)
               (= *ln* !))))
      15 ; Ctrl-O: Page up
        (progn
          (= *ln* (- *ln* (-- *con-h*)))
          (? (< *ln* 0)
             (= *ln* 0))))))

(fn edit file
  (reset-ln)
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
