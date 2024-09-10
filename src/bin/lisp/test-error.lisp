(message "Testing an ONERROR handler...")

(var *last-err* nil)

(fn onerror (code toplevel-expr faulty-expr)
  (out "ONERROR handler")(terpri)
  (out "Error code: ")(print code)(terpri)
  (out "REPL: ")(print toplevel-expr)(terpri)
  (out "Faulty: ")(print faulty-expr)(terpri)
  (= *last-err* code)
  (ignore))

(fn terror (x)
  (= onerror nil)
  (error (or x "error expected")))

(message "Testing ERROR_TYPE...")
(+ 's 1)
(or (== *last-err* 1)
    (terror "Adding 1 to symbol should be an error"))

(message "Testing ERROR_ARG_MISSING...")
(((a b)
  a) 1)
(or (== *last-err* 2)
    (terror "Argument missing should be an error"))

(message "Testing ERROR_TAG_MISSING...")
(block t fnord (go foo))
(or (== *last-err* 3)
    (terror "Missing block tag should be an error"))

(message "Testing ERROR_TOO_MANY_ARGS...")
(((a b)
  a) 1 2 3)
(or (== *last-err* 4)
    (terror "Too many arguments should be an error"))

(message "Testing ERROR_NOT_FUNCTION...")
(not-fun)
(or (== *last-err* 5)
    (terror "(not-fun) should be an error"))

(message "Testing ERROR_ARGNAME_TYPE...")
(((1)) nil)
(or (== *last-err* 6)
    (terror "Non-symbol argument name should be an error"))

(message "Testing ERROR_LOST_RETURN...")
((()
  (return nil)))
(or (== *last-err* 13)
    (terror "Lost RETURN should be an error"))

(message "Testing ERROR_LOST_RETURN...")
((()
  (go tag)))
(or (== *last-err* 14)
    (terror "Lost GO should be an error"))

;(message "Testing ERROR_OUT_OF_HEAP...")
;(((q)
;   (block nil
;     tag
;     (= q (cons t q))
;     (out ".")
;     (go tag)))
; nil)
;(or (== *last-err* 7)
;    (terror "Out-of-heap error expected."))

; ERROR_UNKNOWN_TYPE  7 ; Internal error.
; ERROR_NO_PAREN      8 ; When can read from memory.
; ERROR_STALE_PAREN   9 ; When can read from memory.
; ERROR_FILE_MODE     10
; ERROR_USER          11

(car 's)
(or (== *last-err* 1)
    (terror "CAR of non-NIL atom should be an error"))
(cdr 's)
(or (== *last-err* 1)
    (terror "CDR of non-NIL atom should be an error"))
(setcar 's t)
(or (== *last-err* 1)
    (terror "SETCAR of non-NIL atom should be an error"))
(setcdr 's t)
(or (== *last-err* 1)
    (terror "SETCDR of non-NIL atom should be an error"))

(= onerror nil)
(= *universe* (remove 'terror *universe*))
