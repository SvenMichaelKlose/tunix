(in-package compiler/lambda-expand
  '(make-inline-body inline-loca-call
    mk%= export passthrough expr r))

;;;; EXPORT

(def-gensym closure-name ~cl)

(fn export (x)
  (let (name   (closure-name)
        args   (lambda-args x)
        body   (lambda-body x)
        new-fi (create-funinfo :name   name
                               :args   args
                               :parent *fi*))
    (new-fi.make-scope-arg)
    (*tr*.add-closure $((fn ,name ,args ,@body)))
    $(%closure ,name)))


;;;; PASSTHROUGH

(fn passthrough (x)
  (!? (lambda-funinfo x)
      (with-temporary *fi* !
        (copy-lambda x :body (r (lambda-body x))))
      (let (name    (or (lambda-name x)
                        (fi-sym))
            args    (lambda-args x)
            new-fi  (create-funinfo :name   name
                                    :args   args
                                    :parent *fi*))
        (*fi*.var-add name)
        (with-temp *fi* new-fi
          (copy-lambda x :name  name
                         :args  args
                         :body  (r (lambda-body x)))))))


;;;; TOPLEVEL

(fn expr (x)
  (pcase x
    atom x
    unnamed-lambda?
      (? (lambda-export?)
         (export x)
         (passthrough x))
    named-lambda?
      (passthrough x)
    (r x)))

(fn r (x)
  (@ expr x))

(fn lambda-expand (x)
  (with-global-funinfo
    (r x)))

(in-package nil)
