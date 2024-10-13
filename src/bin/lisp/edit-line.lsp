(load "con.lsp")

;;; State

(var *line* nil)
(var *oline* nil)   ; Original line.
(var *mod?* nil)    ; Line modified?
(var *update?* nil) ; Line needs update in screen?
(var *lx* 0)    ; Line X position, relative to *OX*.

;;; Display

(var *spaces* (dup \  *con-w*))

(fn llen (x)
  (? (list? x)
     (length x)
     (slength x)))

(fn update-line (l)
  (when l
    (outlim *con-w*)
    (out l))
  (!? (nthcdr (llen l) *spaces*)
      (out !)))

;;; Line editing

(fn mkcharline ()
  (= *saved?* nil)
  (= *update?* t)
  (= *mod?* t)
  (when (symbol? *line*)
    (= *line* (symbol-name *line*))))

(fn del-char (x)
  (mkcharline)
  (= *line* (nconc (subseq *line* 0 x)
                   (subseq *line* (++ x)))))

(fn go-eol ()
  (let (n (llen *line*))
    (= *lx* (? (> n 0) n 0))))

; Edit line.
; Return new line if an unrelated char has been input.
(fn edit-line (l)
  (= *alv?* nil)
  (con-crs t)
  (con-direct t)
  (= *mod?* nil)
  (= *line* (or l ""))
  (= *oline* *line*)
  (let (x (con-x)
        y (con-y))
    ; Don't have cursor past line end.
    (and (> *lx* (llen *line*))
         (go-eol))
    (while (not (eof))
      (when *update?*
        (con-xy x y)
        (update-line *line*)
        (= *update?* nil))
      (con-xy (+ x *lx*) y)
      (let (len  (llen *line*)
            c    (while (not (eof))
                    (awhen (conin)
                      (return !))))
        (print c)
        ; Insert char and step right.
        (? (and (>= c 32)
                (or (< c 126)
                    (> c +arr-left+)))
           (progn
             (mkcharline)
             (= *line* (? (== 0 *lx*)
                          (nconc (list c) *line*)
                          (!= (cut-at *lx* *line*)
                            (nconc *line* (list c) !))))
             (!++ *lx*))
           (case c
             +arr-left+
               (!-- *lx*)
             +arr-right+
               (!++ *lx*)
             +bs+
               (progn
                 (when (== 0 *lx*)
                   (putback)
                   (return (? *mod?* (symbol *line*) *oline*)))
                 (? (< 0 *lx*)
                    (del-char (!-- *lx*))))
             1 ; Ctrl-A
               (= *lx* 0)
             4 ; Ctrl-D
               (progn
                 (= *mod?* t)
                 (= *update?* t)
                 (= *line* nil)
                 (= *lx* 0))
             5 ; Ctrl-E
               (go-eol)
             ; Put back unknown key and return line.
             (when (or (< c \ )
                       (and (> c 126)
                            (<= c +arr-left+)))
               (putback)
               (return (? *mod?* (symbol *line*) *oline*)))))))))
