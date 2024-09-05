(fn source (x)
  (? (symbol? x)
     (with ((c nil)
            (v (symbol-value x)))
       $(,(?
            (special? x) 'special
            (macro? x) 'macro
            (cons? v) 'fn
            'var)
         ,x ,@v))
     x))
