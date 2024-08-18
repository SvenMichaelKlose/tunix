(load "while.lisp")

(fn conloop ()
  (while t
    (let c (conin)
      (or (== 0 c)
          (out c)))))
