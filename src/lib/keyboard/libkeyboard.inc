; ##################
; ### Scan codes ###
; ##################

; 00 1           10 [CTRL]   20 [SPACE]  30 Q
; 01 3           11 A        21 Z        31 E
; 02 5           12 D        22 C        32 T
; 03 7           13 G        23 B        33 U
; 04 9           14 J        24 M        34 O
; 05 +           15 L        25 .        35 @
; 06 £           16 ;        26 [RSHIFT] 36 [ARR UP]
; 07 [DEL]       17 [CSR R]  27 [F1]     37 [F5]
; 08 [ARR LEFT]  18 [STOP]   28 [C=]     38 2
; 09 W           19 [LSHIFT] 29 S        39 4
; 0A R           1A X        2A F        3A 6
; 0B Y           1B V        2B H        3B 8
; 0C I           1C N        2C K        3C 0
; 0D P           1D ,        2D :        3D -
; 0E *           1E /        2E =        3E [HOME]
; 0F [RET]       1F [CSR D]  2F [F3]     3F [F7]

.enum keyboard_scancodes
    KBDSC_1
    KBDSC_3
    KBDSC_5
    KBDSC_7
    KBDSC_9
    KBDSC_PLUS
    KBDSC_POUND
    KBDSC_DELETE
    KBDSC_BACKSPACE
    KBDSC_W
    KBDSC_R
    KBDSC_Y
    KBDSC_I
    KBDSC_P
    KBDSC_MULTIPLY
    KBDSC_RETURN
    KBDSC_CTRL
    KBDSC_A
    KBDSC_D
    KBDSC_G
    KBDSC_J
    KBDSC_L
    KBDSC_SEMICOLON
    KBDSC_CURSOR_RIGHT
    KBDSC_STOP
    KBDSC_SHIFT_LEFT
    KBDSC_X
    KBDSC_V
    KBDSC_N
    KBDSC_COMMA
    KBDSC_SLASH
    KBDSC_CURSOR_DOWN
    KBDSC_SPACE
    KBDSC_Z
    KBDSC_C
    KBDSC_B
    KBDSC_M
    KBDSC_PERIOD
    KBDSC_SHIFT_RIGHT
    KBDSC_F1
    KBDSC_COMMODORE
    KBDSC_S
    KBDSC_F
    KBDSC_H
    KBDSC_K
    KBDSC_COLON
    KBDSC_EQUAL
    KBDSC_F3
    KBDSC_Q
    KBDSC_E
    KBDSC_T
    KBDSC_U
    KBDSC_O
    KBDSC_AT
    KBDSC_ARRAY_UP
    KBDSC_F5
    KBDSC_2
    KBDSC_4
    KBDSC_6
    KBDSC_8
    KBDSC_0
    KBDSC_MINUS
    KBDSC_HOME
    KBDSC_F7
.endenum

; KEY CODES

TTY_ENTER           = 13
TTY_ESCAPE          = $f4
TTY_CURSOR_UP       = 5
TTY_CURSOR_DOWN     = 20
TTY_CURSOR_LEFT     = 19
TTY_CURSOR_RIGHT    = 4
TTY_BACKSPACE       = 8
