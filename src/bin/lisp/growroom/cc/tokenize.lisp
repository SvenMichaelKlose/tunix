; Separate input stream into known strings ("tokens") and
; single characters surrounding them.

(fn <=-length (a b)
  (<= (length a) (length b)))

(var *tokens* (sort (read-file "tokens.lisp")
                    '((a b)
                       (<=-length a. b.))))
(var *buffer* nil)
(var *min-buffer-length* (length *tokens*.))

(fn fill-buffer ()
  (dotimes ((- *min-buffer-length* (length *buffer*)))
    (enqueue *buffer* (in))))

(fn consume (len)
  (prog1 (subseq *buffer* 0 len)
    (= *buffer* (make-queue (subseq *buffer* len)))))

(fn token? (name)
  (head? name buffer))

(fn token ()
  (fill-buffer)
  (!? (find-if token? *tokens*))
      (consume (length !.)))

(fn tokenize ()
  (!? (or (token)
          (queue-pop *buffer*))
      (. ! (tokenkize))))
