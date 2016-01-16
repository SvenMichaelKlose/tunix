(= *model* :vic-20+xk)

(defun make (to files &optional (cmds nil))
  (apply #'assemble-files to files)
  (& cmds (make-vice-commands cmds "break .stop")))

(defun make-program ()
  (make "utilgui"
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
             "patterns.asm"
             "boot.asm"
             "compress-font.asm"
             "kernal-end.asm"))
        "utilgui.vice.txt"))

(make-program)
(quit)
