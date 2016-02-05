(= *model* :vic-20+xk)

(load "syscalls.lisp")

(defun make (to files &optional (cmds nil))
  (apply #'assemble-files to files)
  (& cmds (make-vice-commands cmds "break .stop")))

(defun make-program ()
  (make "ultigui"
        (@ [+ "src/" _]
           '("../bender/vic-20/vic.asm"
             "zeropage.asm"
             "../bender/vic-20/basic-loader.asm"
             "main.asm"
             "kernal-start.asm"
             "masks.asm"
             "patterns.asm"
             "syscalls.asm"
             "interpreter.asm"
             "bytecode.asm"
             "calcscr.asm"
             "reset-region.asm"
             "clip.asm"
             "vline.asm"
             "vfill.asm"
             "hline.asm"
             "frame.asm"
             "box.asm"
             "putchar.asm"
             "putstring.asm"
             "gfx-init.asm"
             "window.asm"
             "boot.asm"
             "compress-font.asm"
             "kernal-end.asm"))
        "ultigui.vice.txt"))

(make-program)
(quit)
