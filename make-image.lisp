(defun write-zeroes (x o)
  (adotimes x
    (write-byte 0 o)))

(defun write-block (x o)
  (& x
     (princ x o))
  (write-zeroes (- (* 8 1024) (? x
                                 (length x)
                                 0))
                o))

(defvar *img-blocks* (/ 8192 8))

(defvar *fs-block-size* 10) ; 1Kb
(defvar *fs-blocks* (/ 8192 2))

(defun make-image ()
  (with-output-file o "compiled/ultimem.img"
    (write-block (+ (fetch-file "src/flashboot/flashboot.bin")
                    (fetch-file "src/flashmenu/flashmenu.bin"))
                 o)
    (adotimes ((- *img-blocks* 1))
      (write-block nil o))))

(make-image)
(quit)
