(defvar *syscalls* nil)
(defvar *bytecodes* nil)

(defun syscallbyindex (x)
  (elt (+ *syscalls* *bytecodes*) x))

(defun syscall-name (x)
  (downcase (symbol-name x.)))

(defun syscall-bytecodes-source ()
  (apply #'+ (maptimes [format nil "c_~A=~A~%" (syscall-name (syscallbyindex _)) (++ _)]
                       (length (+ *syscalls* *bytecodes*)))))

(defun syscall-bytecodes ()
  (apply #'+ (maptimes [asm (format nil "c_~A=~A" (syscall-name (syscallbyindex _)) (++ _))]
                       (length (+ *syscalls* *bytecodes*)))))

(defun syscall-vectors (label prefix)
  (+ (asm label)
     (mapcan [asm (format nil "~A~A" prefix (syscall-name _))] (+ *syscalls* *bytecodes*))))

(defun syscall-vectors-l () (syscall-vectors "syscall_vectors_l:" "<"))
(defun syscall-vectors-h () (syscall-vectors "syscall_vectors_h:" ">"))
(defun syscall-args-l () (syscall-vectors "syscall_args_l:" "<args_"))
(defun syscall-args-h () (syscall-vectors "syscall_args_h:" ">args_"))

(defun syscall-args ()
  (mapcan [+ (asm (format nil "args_~A:" (syscall-name _)))
             (asm (princ (length ._) nil))
             (mapcan [asm (downcase (symbol-name _))] ._)]
          (+ *syscalls* *bytecodes*)))

(defmacro define-syscall (name &rest args)
  (| (assoc name *syscalls*)
     (acons! name args *syscalls*))
  nil)

(defmacro define-bytecode (name &rest args)
  (| (assoc name *bytecodes*)
     (acons! name args *bytecodes*))
  nil)

(defun syscall-table ()
  (mapcan [asm (format nil "jmp ~A" (syscall-name _))] *syscalls*))

;;;;;;;;;;;;;;;;;;;
;;; Moving data ;;;
;;;;;;;;;;;;;;;;;;;

(define-bytecode setzb tmp tmp2)
(define-bytecode setzw tmp tmp2 tmp3)
(define-bytecode setzs d tmp)

;;;;;;;;;;;;;;;;;;;
;;; Arithmetics ;;;
;;;;;;;;;;;;;;;;;;;

(define-bytecode addzb tmp tmp2 tmp3)
(define-bytecode sbczb tmp tmp2 tmp3)
;(define-bytecode addzbi tmp tmp2 tmp3)
(define-bytecode sbczbi tmp tmp2)
(define-bytecode addx tmp)
(define-bytecode addy tmp)

;;;;;;;;;;;;;
;;; Stack ;;;
;;;;;;;;;;;;;

(define-bytecode pushz tmp tmp2)
(define-bytecode popz tmp tmp2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphics primitives ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syscall calcscr xpos ypos)
(define-syscall setpattern pattern patternh)
(define-syscall vline xpos ypos height)
(define-syscall hline xpos ypos width)
(define-syscall frame xpos ypos width height)
(define-syscall box xpos ypos width height)
(define-syscall putstring p ph)
(define-syscall putchar)

;;;;;;;;;;;;;;;;;;;;;;
;;; Function calls :::
;;;;;;;;;;;;;;;;;;;;;;
(define-bytecode apply)

(= *syscalls* (reverse *syscalls*))
(= *bytecodes* (reverse *bytecodes*))

(with-output-file o "bytecodes.asm"
  (princ (syscall-bytecodes-source) o))
