(= *model* :vic-20)

(load "syscalls.lisp")

(defun make (to files &optional (cmds nil))
  (apply #'assemble-files to files)
  (& cmds (make-vice-commands cmds "break .stop")))

(defun make-core ()
  (make "compiled/g"
        (@ [+ "src/" _]
           '("../bender/vic-20/vic.asm"
             "zeropage.asm"
             "../bender/vic-20/basic-loader.asm"
             "core/main.asm"
             "init/main.asm"
             "core/kernal-start.asm"
             "core/0400.asm"
             "core/alloc-bank.asm"
             "core/make-jump-table.asm"
             "core/load.asm"
             "core/switch-banks-in.asm"
             "core/moveram.asm"
             "core/pointer-manipulation.asm"
             "core/syscall-index.asm"
             "core/kernal-end.asm"
             "core/process-data.asm"))
        "obj/g.vice.txt"))

(defun make-gfx ()
  (make "compiled/lib/gfx"
        (@ [+ "src/" _]
           '("../bender/vic-20/vic.asm"
             "zeropage.asm"
             "gfx/main.asm"
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
             "gfx/compress-font.asm"
             "gfx/init.asm"
             "bytecode/interpreter.asm"
             "bytecode/instructions.asm"))
        "obj/gfx.vice.txt"))

(make-core)
(make-gfx)
(quit)
