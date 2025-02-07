; ⚠️  UNDER CONSTRUCTION ⚠️
; File selector box

(load 'con.lsp)

(var *spaces* (dup \  *con-w*))

(fn clr-eol ()
  (out (nthcdr (con-x) *spaces*)))

(fn fsb-print-heading ()
  (con-xy 0 0)
  (con-rvs t)
  (out "File selection")
  (clr-eol)
  (con-rvs nil))

(fn fsb-redraw (files ofs)
  (fsb-print-heading)
  (do ((i (subseq files ofs (+ ofs (- *con-h* 2))) (cdr i))
       (y 1 (++ y)))
      ((not i))
    (con-xy 0 y)
    (out (caar i))
    (clr-eol))
  (con-xy 0 (-- *con-h*))
  (out "Arrows: up/down"))

(fn fsb-redraw-item (files ofs idx selected?)
  (con-xy 0 (++ (- selected ofs)))
  (con-rvs selected?)
  (out (nth idx files))
  (clr-eol)
  (con-rvs nil))

(fn fsb ()
  (let (files     (ls)
        ofs       0
        selected  0)
    (con-direct t)
    (fsb-redraw files ofs)
    (while (not (eof))
      (fsb-redraw-item files ofs selected t)
      (case (conin)
        *arr-up*
          (when (< 0 selected)
            (fsb-redraw-item files ofs selected nil)
            (--! selected))
        *arr-down*
          (when (< selected (-- (length files)))
            (fsb-redraw-item files ofs selected nil)
            (++! selected))
        *enter*
          (return nil)))
    (con-direct nil)
    (nth selected files)))
