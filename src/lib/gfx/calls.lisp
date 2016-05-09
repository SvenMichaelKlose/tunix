(defvar *syscalls* nil)
(defvar *bytecodes* nil)

(defun syscallbyindex (x)
  (elt (+ *syscalls* *bytecodes*) x))

(defun syscall-name (x)
  (downcase (symbol-name x.)))

(defun syscall-bytecodes-source ()
  (apply #'+ (maptimes [format nil "c_~A=~A~%.export c_~A~%" (syscall-name (syscallbyindex _)) (++ _) (syscall-name (syscallbyindex _))]
                       (length (+ *syscalls* *bytecodes*)))))

(defun syscall-vectors (label prefix)
  (+ (format nil ".export ~A~%" label)
	 label ": "
     " .byte "
     (apply #'+ (pad (mapcar [format nil "~A~A" prefix (syscall-name _)]
                             (+ *syscalls* *bytecodes*))
                     ", "))
     (format nil "~%")))

(defun syscall-vectors-l () (syscall-vectors "syscall_vectors_l" "<"))
(defun syscall-vectors-h () (syscall-vectors "syscall_vectors_h" ">"))
(defun syscall-args-l () (syscall-vectors "syscall_args_l" "<args_"))
(defun syscall-args-h () (syscall-vectors "syscall_args_h" ">args_"))

(defun syscall-args ()
  (apply #'+ (mapcar [+ (format nil ".export args_~A~%" (syscall-name _))
					    (format nil "args_~A: .byte " (syscall-name _))
                        (apply #'+ (pad (+ (list (princ (length ._) nil))
									       (mapcar [downcase (symbol-name _)] ._))
										", "))
						(format nil "~%")]
                     (+ *syscalls* *bytecodes*))))

(defun syscall-imports ()
  (+ (format nil ".importzp d, tmp, tmp2, tmp3, ph, p, width, height, patternh, pattern, xpos, ypos~%")
	 (apply #'+ ".import "
		        (pad (mapcar [format nil "~A" (syscall-name _)]
                             (print (+ *syscalls* *bytecodes*)))
				     ", "))
	 (format nil "~%")))

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
(define-syscall setpattern pattern pattern+1)
(define-syscall vline xpos ypos height)
(define-syscall hline xpos ypos width)
(define-syscall frame xpos ypos width height)
(define-syscall box xpos ypos width height)
(define-syscall putstring p ph)
(define-syscall putchar)
(define-syscall get_text_width s s+1)

;;;;;;;;;;;;;;;;;;;;;;
;;; Function calls :::
;;;;;;;;;;;;;;;;;;;;;;
(define-bytecode apply)

(= *syscalls* (reverse *syscalls*))
(= *bytecodes* (reverse *bytecodes*))
