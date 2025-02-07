; TUNIX Lisp bytecode interpreter for the 6502-CPU
; Requiring 1 byte of CPU stack.
;
; ⚠️  UNDER CONSTRUCTION ⚠️
;

; FUNCTION LAYOUT (symbol name)
;
; $000: Stack frame size in bytes
; $001: Bytecode offset
; $0xx-0xx: Pointers to objects and built-ins
; $0xx-$101: Bytecodes

; BYTECODES:
;
; %1bsiiiii: Function call (built-in/bytecode)
;            b: 0: built-in, 1: bytecode
;            s: 1: push result onto stack
;            i: Index of pointer to object/built-in,
;               relative to function start.
; %9???????: Internal code
; %000iiiii: Get stack at index+1
; %001iiiii: Set stack at index
; %010.....: Jump
; %0110....: Jump if NIL
; %0111....: Jump if not NIL
; %00000000: Function return

start_bytecode:
    pha
    (pushi 0)   ; A macro expanded with regular macro expander.
    pla

bytecode:
    sta bcf
    stx (++ bcf)

    ; Add stack frame.
    ldy #1
    lda (bcf),y
    sec
    sbc spl
    sta spl
    bcs n
    dec sph

    ; Get first code.
n:  ldy #2
    lda (bcf),y
    clc
    adc bcf
    sta bcp
    ldy (++ bcf)
    bcs n
    iny
n:  sty (++ bcp)
    beq run_code    ; (jmp)

bc_getstack:
    ; Fetch index.
    ldy #1
    lda (bcp),y
    tay

    ; Copy stack at index to top of stack.
    lda spl
    sta sp
    lda (sp),y
    tax
    iny
    lda (sp),y
    ldy #1
    lda (sp),y
    dey
    txa
    sta (sp),y
    lda #0
    sta spl
    beq next2   ; (jmp)

bc_setstack:
    ; Read referenced object to tmp.
    and #$3f
    tay
    lda (bcf),y
    sta tmp
    iny
    lda (bcf),y
    sta (++ tmp)

    ; Fetch index.
    ldy #1
    lda (bcp),y

    ; Copy tmp to stack at index.
    tay
    lda spl
    sta sp
    lda tmp
    sta (sp),y
    iny
    lda (++ tmp)
    sta (sp),y
    lda #0
    sta spl

next2:
    lda #2
    clc
    adc bcp
    sta bcp
    bcc run_code
    inc (++ bcp)
    
run_code:
    ; Fetch code
    ldy #0
    lda (bcp),y

    ; Dispatch call/code.
    cmp #BC_CALL
    bcs internal_code

    ; Dispatch call of built-in/bytecode.
    cmp #BC_BUILTIN

    ; Get object from function's table.
    and #31
    tay
    lda (bcf),y
    sta (+ 1 fun)
    iny
    lda (bcf),y
    sta (+ 2 fun)

    ; Divert to bytecode call...
    bcc call_user

fun:jsr $dead

store_ax:
    ; Save for PUSH or conditional jump following.
    sta last_result
    stx (++ last_result)

    ; Push AX onto stack if desired.
    ldy #0
    lda (bcp),y
    and #BC_PUSH
    beq n
    ldy spl
    bne l3
    dec sph
l3: dey
    lda (++ last_result)
    sta (sp),y
    dey
    lda last_result
    sta (sp),y
    sty spl

    ; Step to next code.
n:  inc bcp
    bne run_code
    inc (++ bcp)
    bne run_code        ; (jmp)

call_user:
    inc bcp
    bne n
    inc (++ bcp)
n:  (pushtag bcp)
    (push bcf)
    lda (+ 1 fun)
    ldx (+ 2 fun)
    jmp bytecode

internal_code:
    and #7
    tay
    lda icodes_l,y
    sta (+ 1 fun2)
fun2:
    jmp $beef   ; All handlers on same page!

return:
    pha
    txa
    pha

    ; Pop function pointers.
    (pop bcf)
    lda (++ bcf)
    beq real_return
    (poptag bcp)

    ; Remove stack frame.
    ldy #BC_FRAME
    lda spl
    clc
    adc (bcf),y
    sta spl
    bcc n
    inc (++ sp)

n:  pla
    tax
    pla
    jmp store_ax

bc_go_not_nil:
    lda (++ last_result)
    beq next2
    bne bc_go2

bc_go_nil:
    lda (++ last_result)
    bne next2

; Set code pointer relative to function start.
bc_go:
    ldy #1
bc_go2:
    lda bcf
    clc
    adc (bcp),y
    sta bcf
    lda (++ bcf)
    adc #0
    sta (++ bcf)
    jmp next2

real_return:
    pla
    tax
    pla
    rts
