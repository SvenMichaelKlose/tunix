(fn cadr (x)
  (car (cdr x)))

(fn cddr (x)
  (cdr (cdr x)))

(fn length (x)
  (? (cons? x)
     (+ 1 (length (cdr x)))
     0))

(var c 400)

(fn block-test ()
  (out "Looping...")(terpri)
  (block nil
    tag
    (setq c (-- c))
    (? (== 0 (% c 100))
       (print c))
    (? (not (== c 0))
       (go 'tag))))

(fn test-file-write ()
  (open 4 "@:GEN,S,W")
  (setout 4)
  (out "test")
  (close 4)
  (setout stdout))

(gc)
