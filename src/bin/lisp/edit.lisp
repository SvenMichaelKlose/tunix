;;; State

(var *filename* "edit-help.md")
(var *lines* nil)
(var *saved?* nil)  ; Modifications saved?
(var *err* nil)
(var *ln* 0)
(var *conln* 0) ; # of first displayed line.
(var *old-conln* 0)
(var *old-ln* 0)
(var *update-ln* 0)

(fn reset-ln ()
  (= *lx* 0)
  (= *ln* 0)
  (= *conln* 0)
  (= *old-conln* -1)
  (= *old-ln* -1)
  (= *update-ln* nil)
  (= *saved?* t))

;;; Display

(fn print-lines (s)
  (with ((y s)
         (l (nthcdr (+ *conln* s) *lines*)))
    (dotimes (i (- (-- *con-h*) s))
      (con-xy 0 y)
      (update-line (and l (car l)))
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
  (con-direct t)
  (con-xy 0 (-- *con-h*))
  (outrvs *spaces*))

(fn prompt (msg)
  (con-direct t)
  (clr-status)
  (con-xy 0 (-- *con-h*))
  (outrvs msg))

(fn status (msg)
  (prompt msg)
  (con-direct nil)
  (con-xy 0 0))

(fn prompt-ok ()
  (prompt-in "Hit ENTER:" ""))

(fn prompt-in (msg l)
  (prompt msg)
  (with-global *lx* (slength l)
    (prog1
      (while t
        (!= (edit-line l)
          (con-xy 0 0)
          (and (== (conin) +enter+)
               (return !))
          (= l !)))
      (clr-status))))

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
    (status "...")
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
         (status "...")
         (= *filename* f)
         (= *lines* nil)
         (= *lines* (read-lines))
         (reset-ln)
         (clr-status))
       (= *err* "Cannot load."))))

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
  (while (not (eof))
    (update-screen)
    (status-msg)
    (status-pos)
    (let line (nthcdr *ln* *lines*)
      (con-xy 0 (- *ln* *conln*))
      (!= (edit-line (car line))
        (and *mod?*
             (= *saved?* nil))
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
      11 ; Ctrl+K: Command char following.
        (and (eq 'quit (editor-cmds))
             (return nil))
      12 ; Ctrl-L: Redraw screen.
        (clrscr)
      14 ; Ctrl-N: Page down.
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
