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
             "syscalls.asm"
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
             "gfx/init.asm"
             "bytecode/interpreter.asm"
             "bytecode/instructions.asm"
             "window.asm"
             "boot.asm"
             "gfx/compress-font.asm"
             "kernal-end.asm"))
        "ultigui.vice.txt"))

(make-program)
(quit)
