(fn source (x)
  (? (symbol? x)
     (with ((c nil)
            (v (value x)))
       $(,(?
            (special? x) 'special
            (macro? x) 'macro
            (cons? v) 'fn
            'var)
         ,x ,@v))
     x))
