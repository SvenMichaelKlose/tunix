; For "jsr-threaded" 6502 code.
;
; Required procs:
; __enter, __leave.
; __stack, __obj, __ax2ref, __pushax
; __call, __call_native,
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
 $(lda #,.x.
   jsr ,(? (%s? x)
           '__stack
           '__obj)))

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
  (!= (length *fi*.args)
    $(,@(@ arg (cdr .x.))
      ,@(!? (native-fun (car .x.))
            $(jsr ,!)
            (call-lisp x))
      ldy #,(+ ! (cadr x.))
      jsr __ax2ref
      ,@(leave !))))

(fn expr (x)
  (? (%ref? x.)
     (? (last? .x.)
        (assignment x)
        (call x))))

(fn compiler/gen-6502 (fi x)
  (with-global *fi* fi
    (!= (length fi.vars)
      $(,(native-fun fi.name):
        lda #,!
        jsr __enter
        ,@(@ expr x)
        ldy #,!
        jmp __leave)))

(in-package nil)
