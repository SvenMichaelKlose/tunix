; "JSR-threaded" 6502 code generator
;
; Required procs:
;
; * __enter:
;   Subtract A from 'sp'.
; * __leave:
;   Add Y to 'sp', keeping AX.
; * __stack2ax:
;   Read 'sp' + A to AX.
; * __ax2stack:
;   Write AX to 'sp' + Y.
; * __pushax:
;   Push AX onto the stack.
; * __call:
;   Call bytecode/builtin function.
;
; Beware of the limited CPU stack
; spoiling the show!

(in-package 'c/6502
  '(ofs ref2ax assignment call-bytecode
    call-native arg expr))

(fn last? (x)
  (== 128 (bit-and x 128)))

(fn ofs (x)
  (bit-and x 126))

(fn ref2ax (x)
  (? (%s? x)
     $(lda #,.x.
       jsr __stack2ax)
     (!= (objptr x)
       $(lda #<,!
         ldx #>,!))))

(fn assignment (x)
  $(,@(ref2ax .x.)
    ,@(ax2ref x.)))

(fn call-lisp (x)
  $(,@(ref2ax (car .x.))
    jsr __call))

(fn arg (x)
  $(,@(ref2ax x)
    jsr __pushax))

(fn call (x)
  (!= (* 2 (length *fi*.args))
    $(,@(@ arg (cdr .x.))
      ,@(!? (native-fun (car .x.))
            $(jsr ,!)
            (call-lisp x))
      ldy #,(+ ! (cadr x.))
      jsr __ax2stack
      ,@(leave !))))

(fn expr (x)
  (? (%ref? x.)
     (? (last? .x.)
        (assignment x)
        (call x))))

(fn compiler/gen-6502 (fi x)
  (with-global *fi* fi
    (!= (* 2 (length fi.vars))
      $(,(native-fun fi.name):
        lda #,!
        jsr __enter
        ,@(@ expr x)
        ldy #,!
        jmp __leave)))

(in-package nil)
