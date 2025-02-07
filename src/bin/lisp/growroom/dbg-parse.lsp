; Read cc65 .dbg file(?)
; Good luck! (It's growroom stuff.)

(fn dbg-parses (v x)
  (do ((c x .c))
      ((not c))
    (enqueue v x.)
    (when (== c 34) ; \"
      (return c))))

(fn dbg-parsel ()
  ; Split line at commas, skipping over quoted strings
  (do ((q (make-queue))
       (v (make-queue))
       (c x .c))
      ((not c)
       (enqueue q v)
       (queue-list q))
    (?
      (== c \,)
        (enqueue q v)
      (== c 34) ; \"
        (= c (dbg-parses v c))
      (progn
        (enqueue v !)
        (= v (make-queue))))))

(fn dbg-parse ()
  (read-line)   ; Version
  (with (q (make-queue)
         sections nil)
    (awhile (symbol-name (read-line))
      (let* (p      (position \  !)
             s      (symbol (subseq ! 0 p))
             sa     (assoc s sections)
             fields (dbg-parsel (member-if '((c) (not (== \  c))) !)))
        (? sa
           (setcdr sa (. fields (cdr sa)))
           (acons! s (list fields) sections))))
    (queue-list q)))
