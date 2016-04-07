(= *model* :vic-20)

(defun make (to files &optional (cmds nil))
  (apply #'assemble-files to files)
  (& cmds (make-vice-commands cmds "break .stop")))

(defun make-core ()
  (make "obj/core"
        (@ [+ "src/" _]
           '("../bender/vic-20/vic.asm"
             "zeropage.asm"
             "lib/gfx/vic-settings.inc.asm"
             "core/errno.inc.asm"

             "core/main.asm"

             "core/low-data.asm"

             "core/mem/pointer-manipulation.asm"
             "core/mem/moveram.asm"
             "core/mem/clrram.asm"
             "core/mem/alloc-bank.asm"

             "core/string.asm"

             "core/proc/switch-banks-in.asm"
             "core/proc/switch.asm"
             "core/proc/process.asm"

             "core/fs/vfile.asm"
             "core/fs/open.asm"
             "core/fs/fs.asm"

             "core/dev/kbd.asm"
             "lib/gfx/calcscr.asm"
             "lib/gfx/clear-screen.asm"
             "lib/gfx/init-bitmap-mode.asm"
             "core/dev/con/charset-4x8.asm"
             "core/dev/con/print.asm"
             "core/dev/con/main.asm"
             "core/dev/cbm/kernal.asm"
             "core/dev/cbm/control.asm"
             "core/dev/cbm/directory.asm"

             "core/load.asm"
             "core/make-jump-table.asm"
             "core/link.asm"
             "core/launch.asm"

             "core/guru-meditation.asm"

             "core/syscall-index.asm"

             "init/main.asm"

             "core/process-data.asm"))
        "compiled/core.vice.txt")
  (alet (get-label 'library_calls)
    (format t "Number of possible linked library functions per process: ~A~%" (integer (/ (- #xfff !) 9)))))

(load "src/lib/gfx/calls.lisp")

(defun make-gfx ()
  (make "compiled//gfx"
        (@ [+ "src/" _]
           `("../bender/vic-20/vic.asm"
             "zeropage.asm"
             ,@(@ [+ "lib/gfx/" _]
                  '("zeropage.asm"
                    "vic-settings.inc.asm"
                    "start.asm"
                    "main.asm"
                    "masks.asm"
                    "patterns.asm"
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
                    "compress-font.asm"
                    "init-bitmap-mode.asm"
                    "init.asm"))
             "ui/syscalls.asm"
             "ui/window.asm"
             "ui/main.asm"
             "bytecode/interpreter.asm"
             "bytecode/instructions.asm"
             "lib/gfx/end.asm"))
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

(make-gfx)
(make-core)
(make-image)
(quit)
