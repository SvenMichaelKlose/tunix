(= *model* :vic-20+xk)

(defun make (to files &optional (cmds nil))
  (apply #'assemble-files to files)
  (& cmds (make-vice-commands cmds "break .stop")))

(defun make-program ()
  (make "g.prg"
        (@ [+ "src/" _]
           '("../bender/vic-20/vic.asm"
             "zeropage.asm"
             "../bender/vic-20/basic-loader.asm"
             "main.asm"
             "kernal-start.asm"
             "reset.asm"
             "gfx-init.asm"
             "fill-column.asm"
             "hline.asm"
             "kernal-end.asm"))
        "g.prg.vice.txt"))

(make-program)
(quit)
