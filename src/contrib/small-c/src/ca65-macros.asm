; UNDER ACTIVE CONSTRUCTION!

.macro seg_code
    .code
.endmacro

.macro seg_data
    .data
.endmacro

.macro datab x
    .byte x
.endmacro

.macro dataw x
    .word x
.endmacro

.macro bssb
    .res 1
.endmacro

.macro defglobal name
name:
.endmacro

.macro deflocal n
@n:
.endmacro

#define IR_LOCAL 8
#define IR_GLOBAL 9
#define IR_IMPORT 10
#define IR_EXPORT 11
#define IR_SWAP 12
#define IR_LDAL 13
#define IR_LDA 14
#define IR_LDB 15
#define IR_ADDSP 16
#define IR_ADDA 17
#define IR_ADDB 18
#define IR_INCA 19
#define IR_DECA 20
#define IR_DECB 21
#define IR_SIGNEXT 22
#define IR_STA 23
#define IR_STAL 24
#define IR_PUTCHAR 25
#define IR_PUTINT 26
#define IR_GETCHAR 27
#define IR_GETUCHAR 28
#define IR_GETINT 29
#define IR_PUSHA 30

.macro pusha
    lda regal
    pha
    lda regah
    pha
.endmacro

.macro pushb
    lda regbl
    pha
    lda regbh
    pha
.endmacro

.macro popa
    pla
    sta regah
    pla
    sta regal
.endmacro

.macro popb
    pla
    sta regbh
    pla
    sta regbl
.endmacro

#define IR_SWAPSTACK 34
#define IR_INCS1 35
#define IR_INCS2 36
#define IR_DECS1 37
#define IR_DECS2 38
#define IR_SPHL 39
#define IR_CALL 40
#define IR_CALLPTR 41
#define IR_RET 42

.macro jmp local
    jmp @local
.endmacro

.macro jmpnz local
    lda regal
    ora regah
    beq :+
    jmp @local
:
.endmacro

.macro jmpz local
    lda regal
    ora regah
    bne :+
    jmp @local
:
.endmacro

#define IR_JMPCASE 46

; A boolean is a boolean is a boolean.
.macro bool
.endmacro

#define IR_LNEG 48
#define IR_NEG 49
#define IR_ASLA 50
#define IR_ASL 51
#define IR_ASR 52
#define IR_LSR 53

; Fast version
.macro adda
    lda regal
    clc
    adc regbl
    sta regal
    lda regah
    adc regbl
    sta regah
.endmacro

; Slow version
.macro adda
    jsr _adda_
.endmacro

#define IR_ADDB 55
#define IR_SUB 56
#define IR_MUL 57
#define IR_DIV 58
#define IR_UDIV 59
#define IR_AND 60
#define IR_OR 61
#define IR_XOR 62
#define IR_COMPA 63
#define IR_COMPB 64
#define IR_EQ 65
#define IR_NE 66
#define IR_LT 67
#define IR_LTE 68
#define IR_GT 69
#define IR_GTE 70
#define IR_ULT 71
#define IR_ULTE 72
#define IR_UGT 73
#define IR_UGTE 74
#define IR_SRCLINE 75
