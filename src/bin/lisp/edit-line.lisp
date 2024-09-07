(load "let.lisp")
(load "aif.lisp")
(load "alet.lisp")
(load "incdec.lisp")
(load "cbm-keycode.lisp")
(load "con-cbm.lisp")
(load "con.lisp")

(load "do.lisp")
(load "prog1.lisp")
(load "progn.lisp")
(load "while.lisp")
(load "when.lisp")
(load "awhen.lisp")
(load "make-queue.lisp")
(load "enqueue.lisp")
(load "queue-list.lisp")
(load "mapcar.lisp")
(load "mapcan.lisp")
(load "cut-at.lisp")
(load "case.lisp")
(load "with-global.lisp")
(load "with-in.lisp")
(load "with-out.lisp")
(load "with-queue.lisp")

;(load "with-global.lisp")

;;; State

(var *line* nil)
(var *oline* nil)   ; Original line.
(var *mod?* nil)    ; Line modified?
(var *err* nil)
(var *ln* 0)
(var *ox* 0)    ; Line X offset.
(var *lx* 0)    ; Line X position, relative to *OX*.
(var *old-conln* 0)
(var *old-ln* 0)

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
(fn edit-line (l)
  (con-crs t)
  (con-direct t)
  (= *mod?* nil)
  (= *oline* l)
  (let y (con-y)
    (= *line* (? l (symbol-name l) nil))
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
                (con-direct nil)
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
              (con-direct nil)
              (return (? *mod?* (symbol *line*) *oline*)))
            ; Insert char and step right.
            (= *mod?* t)
            (= *line* (? (== 0 *lx*)
                         (nconc (list c) *line*)
                         (!= (cut-at *lx* *line*)
                           (nconc *line* (list c) !))))
            (!++ *lx*)))))))
