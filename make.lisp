(= *model* :vic-20)

(defun make (to files &optional (cmds nil))
  (apply #'assemble-files to files)
  (& cmds (make-vice-commands cmds "break .stop")))

(defun make-core ()
  (make "obj/core"
        (@ [+ "src/" _]
           '("zeropage.asm"
             "core/main.asm"

             "core/pointer-manipulation.asm"
             "core/moveram.asm"
             "core/clrram.asm"
             "core/string.asm"

             "core/alloc-bank.asm"

             "core/process.asm"
             "core/task-switching.asm"
             "core/switch-banks-in.asm"

             "core/make-jump-table.asm"
             "core/link.asm"
             "core/load.asm"
             "core/launch.asm"

             "core/syscall-index.asm"

             "init/main.asm"

             "core/process-data.asm"))
        "compiled/core.vice.txt")
  (alet (get-label 'library_calls)
    (format t "Number of possible linked library functions per process: ~A~%" (integer (/ (- #xfff !) 9)))))

(defun make-sh ()
  (make "compiled/sh"
        (@ [+ "src/" _]
           '("zeropage.asm"
             "sh/start.asm"
             "sh/main.asm"
             "sh/print.asm"
             "sh/end.asm"))
        "compiled/sh.vice.txt"))

(defun make-clock ()
  (make "compiled/clock"
        (@ [+ "src/" _]
           '("zeropage.asm"
             "clock/start.asm"
             "clock/main.asm"
             "clock/end.asm"))
        "compiled/clock.vice.txt"))

(load "src/gfx/calls.lisp")

(defun make-gfx ()
  (make "compiled//gfx"
        (@ [+ "src/" _]
           '("../bender/vic-20/vic.asm"
             "zeropage.asm"
             "gfx/zeropage.asm"
             "gfx/start.asm"
             "gfx/main.asm"
             "ui/syscalls.asm"
             "gfx/masks.asm"
             "gfx/patterns.asm"
             "gfx/calcscr.asm"
             "gfx/reset-region.asm"
             "gfx/clip.asm"
             "gfx/vline.asm"
             "gfx/vfill.asm"
             "gfx/hline.asm"
             "gfx/frame.asm"
             "gfx/box.asm"
             "gfx/putchar.asm"
             "gfx/putstring.asm"
             "gfx/compress-font.asm"
             "gfx/init.asm"
             "ui/window.asm"
             "ui/main.asm"
             "bytecode/interpreter.asm"
             "bytecode/instructions.asm"
             "gfx/end.asm"))
        "compiled/gfx.vice.txt"))

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
  (with-output-file o "compiled/g.img"
    (write-block (fetch-file "obj/core") o)
    (adotimes ((- *img-blocks* 1))
      (write-block nil o))))

;(make-sh)
;(make-clock)
(make-gfx)
(make-core)
(make-image)
(quit)
