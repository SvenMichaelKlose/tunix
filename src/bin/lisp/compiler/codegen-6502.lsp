; For "jsr-threaded" 6502 code.
;
; Required procs:
; __enter, __leave.
; __stack, __obj, __ax2ref,
; __call_bytecode, __call_native,
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

(fn call-bytecode (x)
  $(,@(ref2ax (car .x.))
    jsr call_bc))

(fn call-native (x)
  $(,@(ref2ax (car .x.))
    jsr call_native))

(fn arg (x)
  $(,@(ref2ax x)
    jsr pushax))

(fn call (x)
  $(,@(@ arg (cdr .x.))
    ,@(? (bytecode-fun? (car .x.))
         (call-bytecode x)
         (call-native x))
    ldy #,(+ (cadr x.)
             (length *fi*.args))
    jsr __ax2ref
    ,@(leave (length *fi*.args))))


(fn expr (x)
  (? (%ref? x.)
     (? (last? .x.)
        (assignment x)
        (call x))))

(fn compiler/gen-6502 (fi x)
  (with-global *fi* fi
    $(lda #,(length *fi*.vars)
      jsr __enter
      ,@(@ expr x)
      ldy #,(length *fi*.vars))
      jmp __leave))

(in-package nil)
