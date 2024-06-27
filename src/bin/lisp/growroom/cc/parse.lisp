(var *rules* (read-file "rules.lisp"))

(fn find-rule (x)
  (car (assoc x *rules*)))

(fn match-pattern (tokens x path)
  (!?
    (find-rule x)
      (match-rules tokens ! (. x path))
    (eq tokens.. x)
      tokens.))

(fn match-rule (tokens x path matched)
  (unless tokens
    (error "Unexpected end of file."))
  (? x
     (!? (match-pattern tokens x. path)
         (match-rule .tokens path (. ! matched)))
     (reverse matched)))

(fn match-rules (tokens x path)
  (or (match-rule tokens .x path nil)
      (match-rules tokens x.)))

(fn parse (x)
  (fill-buffer)
  (match-rules (car (assoc 'program *rules*))))
