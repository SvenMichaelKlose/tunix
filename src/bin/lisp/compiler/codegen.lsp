(in-package 'c/gc
  '(*bc* gen-bc
    some-%go? map-tags xlat-jumps
    gen-ref gen-funcall gen-body))

(macro defcode (name args . body)
  $(umacro codegen ,name ,args ,@body))

(fn compiler/codegen (x)
  (@ '((_) (umacroexpand codegen _)) x))

(const +bc+
    '((%return  . 255)
      (%go      . 254)
      (%go-nil  . 253)
      (%go-nnil . 252)))

(fn gen-bc (x)
  (cdr (assoc x +bc+)))

(fn some-%go? (x)
  (in? x '%go '%go-nil '%go-nnil))

(fn map-tags (body)
  (do* ((tags nil)
        (pos 0)
        (s body .s))
      ((not s) tags)
    (?
      (%tag? s.)
        (push tags (. .s. pos)))))
      (or (some-%go? s.)
          (%return? s.))
        (+! pos 1)
      (+! pos (length s.))

(fn xlat-jump (tags x)
  (some-%go? x)
    $(,(gen-bc x.)
      ,(cdr (assoc tag tags)))
  x)

(fn xlat-jumps (tags x)
  (@ $((x)
        (xlat-jump ,tags x))
     x))

(defcode lambda (name (args . body))
  (with-global *fi* (get-funinfo name)
    $(,name ,args
      ,*fi*.ssize ,*fi*.objs
      ,(flatten
           (@ codegen
              (xlat-jumps (map-tags x)
                          x))))))

(fn gen-ref (x)
  (? (%s? x)
     (++ .x.) ; LSB means stack.
     (*fi*.obj-idx .x.)))

(fn gen-funcall (x)
  (when x
    (. (+ (? .x 0 128) ; MSB is last.
          (gen-ref x.))
       (gen-funcall .x))))

(defcode %= (place value)
  (.. (gen-ref place)
      (? (.? value)
         (gen-funcall value)
         (gen-ref value))))

(defcode %return ()
  (gen-bc '%return))

(in-package nil)
