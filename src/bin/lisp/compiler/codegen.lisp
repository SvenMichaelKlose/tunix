(defmacro def-php-codegen (name &body body)
  `(define-codegen-macro *php-transpiler* ,name ,@body))


