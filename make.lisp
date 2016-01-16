(= *model* :vic-20+xk)

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
             "gfx-init.asm"
             "calcscr.asm"
             "vline.asm"
             "vfill.asm"
             "hline.asm"
             "frame.asm"
             "box.asm"
             "putchar.asm"
             "moveram.asm"
             "syscalls.asm"
             "script.asm"
             "patterns.asm"
             "boot.asm"
             "compress-font.asm"
             "window.asm"
             "kernal-end.asm"))
        "ultigui.vice.txt"))

(make-program)
(quit)
