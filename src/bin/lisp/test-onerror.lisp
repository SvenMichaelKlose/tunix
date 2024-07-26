(message "Testing an ONERROR handler...")

(var *last-err* nil)

(fn onerror (code toplevel-expr faulty-expr)
  (out "ONERROR handler")(terpri)
  (out "Error code: ")(print code)(terpri)
  (out "REPL: ")(print toplevel-expr)(terpri)
  (out "Faulty: ")(print faulty-expr)(terpri)
  (= *last-err* code)
  (noerror))

(fn terror (x)
  (= onerror nil)
  (error (or x "error expected")))

; ERROR_TYPE          1
(+ 's 1)
(or (== *last-err* 1)
    (terror "Adding 1 to symbol should be an error"))

; ERROR_ARG_MISSING   2
(((a b) a) 1)
(or (== *last-err* 2)
    (terror "Argument missing should be an error"))

; ERROR_TAG_MISSING   3
(block t fnord (go foo))
(or (== *last-err* 3)
    (terror "Missing block tag should be an error"))

; ERROR_TOO_MANY_ARGS 4
(((a b) a) 1 2 3)
(or (== *last-err* 4)
    (terror "Too many arguments should be an error"))

; ERROR_NOT_FUNCTION  5
(not-a-function)
(or (== *last-err* 5)
    (terror "(not-a-function) should be an error"))

; ERROR_OUT_OF_HEAP   6 ; Good luck testing this one.
; ERROR_UNKNOWN_TYPE  7 ; Internal error.
; ERROR_NO_PAREN      8 ; When can read from memory.
; ERROR_STALE_PAREN   9 ; When can read from memory.

; ERROR_CHANNEL       10

; ERROR_FILE          11
; ERROR_USER          12

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
