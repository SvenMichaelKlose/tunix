(var +hotkey+ 11)   ; CBM Ctrl-K

;;; State

(var *filename* "code.lisp")
(var *lines* nil)
(var *saved?* nil)
(var *lx* 0)      ; Line X position.
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

;;; Display

(var *spaces* nil)
(dotimes (i *con-w*)
  (push \  *spaces*))

(fn update-line (l y)
  (con-xy 0 y)
  (when l
    (outlim *con-w*)
    (out l))
  (out (nthcdr (length l) *spaces*)))

(fn update-screen ()
  (let y 0
    (dolist (l (subseq *lines* *conln* (+ *conln* (- *con-h* 2))))
      (update-line (symbol-name l) y)
      (!++ y))))

(fn status ()
  (con-xy 0 (-- *con-h*))
  (con-rvs t)
  (apply out *filename* (list (? (not *lines*) " (new)" "")))
  (out " ")(print (++ *lx*))(out ",")(print (++ *ln*))
  (con-rvs nil))

;;; Line editing

(fn del-char (x)
  (= *saved?* nil)
  (= line (nconc (subseq line 0 x)
                 (subseq line (++ x)))))

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (x)
  (con-crs t)
  (let line (? x (symbol-name x) nil)
    ; Don't have cursor past line end.
    (let n (length line)
      (and (> *lx* n)
           (= *lx* (? (> n 0) (-- n) 0))))
    (while (not (eof))
      (update-line line (- *ln* *conln*))
      (con-xy *lx* (- *ln* *conln*))
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
              (? (<= 0 *lx*)
                 (del-char (!-- *lx*))))
          (progn
            ; Put back unknown key and return line.
            (when (< c \ )
              (putback)
              (return (symbol line)))
            ; Insert char and step right.
            (= *saved?* nil)
            (= line (nconc (subseq line 0 *lx*)
                           (list c)
                           (subseq line *lx*)))
            (!++ *lx*)))))))

;;; File I/O

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
    (@ '((x)
          (out x)
          (terpri))
       *lines*))
  (? (err)
     (and (status "Cannot save.") nil)
     (= *saved?* t)))

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

(fn del-line (ln)
  (= *lines* (nconc (subseq *lines* 0 ln)
                    (subseq *lines* (++ ln)))))

(fn ins-line (l)
  (= *lines* (nconc (subseq *lines* 0 *ln*)
                    (let lc (symbol-name l)
                      (list (symbol (subseq lc 0 *lx*))
                            (symbol (subseq lc *lx*))))
                    (subseq *lines* (++ *ln*))))
  (!++ *ln*)
  (= *lx* 0))

; Navigate up and down lines, catch commands.
(fn edit-lines ()
  (while (not (eof))
    (update-screen)
    (status)

    ; Edit current line.
    (let lcons (nthcdr *ln* *lines*)
      (!= (edit-line (car lcons))
        (case (conin)
          +enter+
            (ins-line !)
          ;+del+
          ; Replace line.
          (progn
            (putback)
            (setcar lcons !)))))

    ; Handle line motion and commands.
    (case (conin)
      +arr-up+
        (? (< 0 *ln*)
           (!-- *ln*))
      +arr-down+
        (? (< *ln* (-- (length *lines*)))
           (!++ *ln*))
      +hotkey+
        (progn
          (status "Command: " "")
          (case (conin)
            \s  (save-file)
            \q  (and (quit-editor)
                     (return nil)))))))

(fn edit file
  (= *lx* 0)
  (= *ln* 0)
  (= *conln* 0)
  (= saved? t)
  ;(= *lines* (read-lines file))
  (= *lines*
     (list
       "
This is a text editor written in TUNIX
Lisp, required to write the bytecode
compiler with no supplementary software,
like emulators in warp mode.  Only Lisps
are powerful enough to bring on seamless
integration with little effort.  But the
debugger has to be picture-book to get
there.

This editor is slower than an East-
Westfalian on a Sunday morning.
Destructive built-ins are the cups of
coffee.  'Destructive' to get around
memory allocation and 'built-in' to make
it fast.  Still, it's pure cc65-compiled
ANSI-C.
"
))
  (clrscr)
  (edit-lines)
  (clrscr))
