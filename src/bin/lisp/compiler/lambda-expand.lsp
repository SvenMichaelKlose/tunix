(require 'compiler/funinfo)
(in-package 'c/le
  '(mk%= inline export expr r))

(fn inline (binding-lambda)
  (fn make-body (places values body)
     (+ (@ $((p v)
              $(%= ,,p ,,v))
           stack-places
           values)
        body))
  (with-binding-lambda (args vals body
                        binding-lambda)
    (with (l (argument-expand
                 'inline-binding-lambda
                 args vals)
           a (@ car l)
           v (@ cdr l))
      (*fi*.add-var a)
      (lambda-expand
          (make-body a v body)))))


(def-gensym closure-name ~cl)

(fn export (x)
  (let (name   (closure-name)
        args   (lambda-args x)
        body   (lambda-body x)
        new-fi (create-funinfo
                   :name   name
                   :args   args
                   :parent *fi*))
    (new-fi.make-scope-arg)
    (*tr*.add-closure
      $((fn ,name ,args ,@body)))
    $(%closure ,name)))


(fn expr (x)
  (pcase x
    atom x
    binding-lambda? (inline x)
    unnamed-lambda? (export x)
    named-lambda?
      (do-lambda x
        :body (r (lambda-body x)))
    (r x)))

(fn r (x)
  (@ expr x))

(fn lambda-expand (x)
  (with-global-funinfo
    (r x)))

(in-package nil)
