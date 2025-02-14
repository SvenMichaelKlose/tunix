(require 'compiler/funinfo)
(in-package 'c/le
  '(make-body inline export expr r))

(fn make-body (args vals body)
  $(%block
     ,@(@ #'((a v)
              $(%= ,a ,v))
          args vals)
     ,@body))

(fn inline (binding-lambda)
  (with-binding-lambda (args vals body
                        binding-lambda)
    (let (l (argument-expand
                'inline-binding-lambda
                args vals)
          a (@ car l))
      (*fi*.add-var a)
      (r (make-body a (@ cdr l)
                    body)))))

(def-gensym closure-name ~closure-)

(fn export (x)
  (let (name (closure-name)
        args (lambda-args x))
    (funinfo-make-scope-arg
        (create-funinfo
            :name   name
            :args   args
            :parent *fi*))
    (transpiler-add-closure *tr*
        $((fn ,name ,args
            ,@(lambda-body x))))
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

(var compiler/lambda-expand r)

(in-package nil)
