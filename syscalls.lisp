(defvar *syscalls* nil)

(defun syscallbyindex (x)
  (elt *syscalls* x))

(defun syscall-name (x)
  (downcase (symbol-name x.)))

(defun syscall-bytecodes ()
  (apply #'+ (maptimes [asm (format nil "c_~A=~A" (syscall-name (syscallbyindex _)) (++ _))]
            (length *syscalls*))))

(defun syscall-vectors (label prefix)
  (+ (asm label)
     (mapcan [asm (format nil "~A~A" prefix (syscall-name _))] *syscalls*)))

(defun syscall-vectors-l () (syscall-vectors "syscall_vectors_l:" "<"))
(defun syscall-vectors-h () (syscall-vectors "syscall_vectors_h:" ">"))
(defun syscall-args-l () (syscall-vectors "syscall_args_l:" "<args_"))
(defun syscall-args-h () (syscall-vectors "syscall_args_h:" ">args_"))

(defun syscall-args ()
  (mapcan [+ (asm (format nil "args_~A:" (syscall-name _)))
             (asm (princ (length ._) nil))
             (mapcan [asm (downcase (symbol-name _))] ._)]
          *syscalls*))

(defmacro define-syscall (name &rest args)
  (| (assoc name *syscalls*)
     (acons! name args *syscalls*))
  nil)

(define-syscall vline xpos ypos height)
(define-syscall hline xpos ypos width)
(define-syscall frame xpos ypos width height)
(define-syscall putstring p ph)
(define-syscall putchar)
(define-syscall box xpos ypos width height)
(define-syscall calcscr xpos ypos)
(define-syscall setpattern pattern patternh)
(define-syscall apply)
(define-syscall addx tmp)
(define-syscall addy tmp)
(define-syscall setzb tmp tmp2)
(define-syscall setzw tmp tmp2 tmp3)
(define-syscall setzs d tmp)
(define-syscall addzb tmp tmp2 tmp3)
(define-syscall sbczb tmp tmp2 tmp3)
;(define-syscall addzbi tmp tmp2 tmp3)
(define-syscall sbczbi tmp tmp2)
