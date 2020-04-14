; Codepage 437

.export charset_4x8, _charset_4x8

charset_4x8:
_charset_4x8:

; NULL
    .byte %1110
    .byte %1010
    .byte %1110
    .byte %0000
    .byte %1110
    .byte %1010
    .byte %1110
    .byte %0000

; Smiley
    .byte %0000
    .byte %1001
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %0110
    .byte %0000
    .byte %0000

; Inverted smiley
    .byte %1111
    .byte %0110
    .byte %1111
    .byte %1111
    .byte %0000
    .byte %1001
    .byte %1111
    .byte %1111

; Heart
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %0100
    .byte %0000

; check
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %1110
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %0000

; clubs
    .byte %0000
    .byte %0110
    .byte %0000
    .byte %1001
    .byte %0000
    .byte %0110
    .byte %1111
    .byte %0000


; spade
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %1111
    .byte %0110
    .byte %1111
    .byte %0000

; disc
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0000
    .byte %0000
    .byte %0000

; inverted disc
    .byte %1111
    .byte %1111
    .byte %1011
    .byte %0001
    .byte %1011
    .byte %1111
    .byte %1111
    .byte %1111

; Circle
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000
    .byte %0000

; Inverted circle
    .byte %1111
    .byte %1011
    .byte %0101
    .byte %0101
    .byte %0101
    .byte %1011
    .byte %1111
    .byte %1111
; Male
    .byte %0000
    .byte %0001
    .byte %0010
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; Female
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0000

; Note
    .byte %0011
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0100
    .byte %0000

; Two notes
    .byte %0111
    .byte %0101
    .byte %0111
    .byte %0101
    .byte %0101
    .byte %1001
    .byte %0010
    .byte %0000

; Star(?)
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %1010

; Triangle right
    .byte %1000
    .byte %1100
    .byte %1110
    .byte %1111
    .byte %1110
    .byte %1100
    .byte %1000
    .byte %0000

; Triangle left
    .byte %0001
    .byte %0011
    .byte %0111
    .byte %1111
    .byte %0111
    .byte %0011
    .byte %0001
    .byte %0000

; Arrow up/down
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %0000

; Double exclamation mark.
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000
    .byte %1010
    .byte %0000


; Line feed
    .byte %1111
    .byte %1011
    .byte %1011
    .byte %1111
    .byte %0011
    .byte %0011
    .byte %0011
    .byte %0000

; §
    .byte %0110
    .byte %1000
    .byte %0100
    .byte %1010
    .byte %0100
    .byte %0010
    .byte %1100
    .byte %0000

; Block
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %0000

; Arrow up/down floored
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %1111

; Arrow up
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100

; Arrow down
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1110
    .byte %0100

; Arrow right
    .byte %0000
    .byte %0100
    .byte %0110
    .byte %1111
    .byte %1111
    .byte %0110
    .byte %0100
    .byte %0000

; Arrow left
    .byte %0000
    .byte %0010
    .byte %0110
    .byte %1111
    .byte %1111
    .byte %0110
    .byte %0010
    .byte %0000

; Whatever
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %1000
    .byte %1111
    .byte %1111
    .byte %0000

; Arrow left/right
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1111
    .byte %1111
    .byte %1010
    .byte %0000
    .byte %0000

; Triangle up
    .byte %0100
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110

; Triangle down
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %1110
    .byte %0100

; space
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; !
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000
    .byte %1000
    .byte %0000

; "
    .byte %1010
    .byte %1010
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; #
    .byte %0000
    .byte %0101
    .byte %1111
    .byte %0101
    .byte %0101
    .byte %1111
    .byte %0101
    .byte %0000

; $
    .byte %0100
    .byte %0110
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %1100
    .byte %0100
    .byte %0000

; %
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1001
    .byte %0010
    .byte %0100
    .byte %1001
    .byte %0000

; &
    .byte %0000
    .byte %0100
    .byte %1000
    .byte %0101
    .byte %1010
    .byte %1010
    .byte %0101
    .byte %0000

; '
    .byte %1000
    .byte %1000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; (
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %0000

; )
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %0000

; *
    .byte %0000
    .byte %1010
    .byte %0100
    .byte %1110
    .byte %1110
    .byte %0100
    .byte %1010
    .byte %0000

; +
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %0000
    .byte %0000

; ,
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %1000

; -
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1100
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; .
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %0000

; /
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000

; 0
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; 1
    .byte %0100
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; 2
    .byte %0100
    .byte %1010
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1110
    .byte %0000

; 3
    .byte %0100
    .byte %1010
    .byte %0010
    .byte %0100
    .byte %0010
    .byte %1010
    .byte %0100
    .byte %0000

; 4
    .byte %1000
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0000

; 5
    .byte %1110
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %0010
    .byte %0010
    .byte %1100
    .byte %0000

; 6
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; 7
    .byte %1110
    .byte %0010
    .byte %0010
    .byte %0100
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %0000

; 8
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; 9
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0110
    .byte %0010
    .byte %1010
    .byte %0100
    .byte %0000

; :
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %0000

; ;
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %1000

; <
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1000
    .byte %0100
    .byte %0000
    .byte %0000

; =
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1100
    .byte %0000
    .byte %1100
    .byte %0000
    .byte %0000

; >
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1000
    .byte %0100
    .byte %1000
    .byte %0000
    .byte %0000

; ?
    .byte %1000
    .byte %0100
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %0000
    .byte %1000
    .byte %0000

; @
    .byte %0000
    .byte %0000
    .byte %0110
    .byte %1110
    .byte %1110
    .byte %1000
    .byte %0110
    .byte %0000

; A
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; B
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %0000

; C
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0100
    .byte %0000

; D
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %0000

; E
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %0000

; F
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000
; G
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0110
    .byte %0000

; H
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; I
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000

; J
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1000
    .byte %0000

; K
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; L
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %0000

; M
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; N
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; O
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; P
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000

; Q
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %0110
    .byte %0000

; R
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; S
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %1010
    .byte %0100
    .byte %0000

; T
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; U
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; V
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1000
    .byte %0000

; W
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %0000

; X
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; Y
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000
; Z
    .byte %1110
    .byte %0010
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1110
    .byte %0000
; [
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %0000

; \
    .byte %0000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; ]
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1100
    .byte %0000

; ^
    .byte %0100
    .byte %1010
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; _
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1110

; `
    .byte %0100
    .byte %0010
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; a
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000

; b
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000
; c
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0100
    .byte %0000

; d
    .byte %0010
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; e
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1000
    .byte %0110
    .byte %0000

; f
    .byte %0100
    .byte %1000
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000

; g
    .byte %0000
    .byte %0000
    .byte %0110
    .byte %1010
    .byte %1010
    .byte %0110
    .byte %0010
    .byte %1100

; h
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; i
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; j
    .byte %0010
    .byte %0000
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %1010
    .byte %0100

; k
    .byte %1000
    .byte %1000
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %0000

; l
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0100
    .byte %0000

; m
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; n
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; o
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; p
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1000

; q
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0110
    .byte %0010

; r
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000
; s
    .byte %0000
    .byte %0000
    .byte %0110
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %1100
    .byte %0000
; t
    .byte %1000
    .byte %1000
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0100
    .byte %0000

; u
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0110
    .byte %0000

; v
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; w
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %0000

; x
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0000

; y
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0110
    .byte %0010
    .byte %1100

; z
    .byte %0000
    .byte %0000
    .byte %1110
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %1110
    .byte %0000

; {
    .byte %0110
    .byte %0100
    .byte %0100
    .byte %1000
    .byte %0100
    .byte %0100
    .byte %0110
    .byte %0000

; |
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100

; }
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0010
    .byte %0100
    .byte %0100
    .byte %1100
    .byte %0000

; ~
    .byte %0101
    .byte %1010
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; House
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %0000

; C cedil
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1000
    .byte %1010
    .byte %0100
    .byte %1100
    .byte %0000

; ü
    .byte %1010
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; é
    .byte %0010
    .byte %0100
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1000
    .byte %0110
    .byte %0000

; A circonflex
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000

; ä
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000

; à
    .byte %1000
    .byte %0100
    .byte %0100
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000

; a°
    .byte %0100
    .byte %1010
    .byte %0100
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000

; c cedil
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %1000

; e circonflex
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1000
    .byte %0110
    .byte %0000

; e umlaut
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1000
    .byte %0110
    .byte %0000

; è
    .byte %1000
    .byte %0100
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1000
    .byte %0110
    .byte %0000

; i umlaut
    .byte %0000
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; i circonflex
    .byte %0100
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; ì
    .byte %1000
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; Ä
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %0000

; A°
    .byte %0100
    .byte %1010
    .byte %0100
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %0000

; É
    .byte %0010
    .byte %0100
    .byte %1110
    .byte %1000
    .byte %1100
    .byte %1000
    .byte %1110
    .byte %0000

; æ
    .byte %0000
    .byte %0000
    .byte %1010
    .byte %0100
    .byte %1110
    .byte %1100
    .byte %1110
    .byte %0000

; Æ
    .byte %0011
    .byte %0110
    .byte %1010
    .byte %1011
    .byte %1110
    .byte %1010
    .byte %1011
    .byte %0000

; o circonflex
    .byte %0100
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; ö
    .byte %1010
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; ò
    .byte %1000
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; u cironflex
    .byte %0100
    .byte %1010
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; ù
    .byte %1000
    .byte %0100
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; y umlaut
    .byte %1010
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %1000

; Ö
    .byte %1010
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; Ü
    .byte %1010
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; cent
    .byte %0100
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %0000

; pound sterling
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1100
    .byte %1000
    .byte %1000
    .byte %1110
    .byte %0000

; yen
    .byte %1010
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %0000

; pesetas
    .byte %1100
    .byte %1010
    .byte %1100
    .byte %1001
    .byte %1011
    .byte %1001
    .byte %0010
    .byte %0000

; florin
    .byte %0010
    .byte %0100
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1000

; á
    .byte %0010
    .byte %0100
    .byte %0100
    .byte %0010
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000

; í
    .byte %0010
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; ó
    .byte %0010
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; ú
    .byte %0010
    .byte %0100
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000

; n~
    .byte %0101
    .byte %1010
    .byte %0000
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000

; N~
    .byte %0101
    .byte %1010
    .byte %0000
    .byte %1010
    .byte %1110
    .byte %1110
    .byte %1010
    .byte %0000

; a_
    .byte %0000
    .byte %0110
    .byte %1010
    .byte %0110
    .byte %0000
    .byte %1110
    .byte %0000
    .byte %0000

; O_
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %0100
    .byte %0000
    .byte %1110
    .byte %0000
    .byte %0000

; ? around
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %1000
    .byte %1010
    .byte %0100
    .byte %0000

; corner top left
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1110
    .byte %1000
    .byte %1000
    .byte %0000
    .byte %0000

; corner top right
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1110
    .byte %0010
    .byte %0010
    .byte %0000
    .byte %0000

; half
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0010
    .byte %0100
    .byte %1110

; quarter
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %0010

; ! around
    .byte %0100
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0000

; double arrow left
    .byte %0000
    .byte %0000
    .byte %0110
    .byte %1100
    .byte %0110
    .byte %0000
    .byte %0000
    .byte %0000

; double arrow right
    .byte %0000
    .byte %0000
    .byte %1100
    .byte %0110
    .byte %1100
    .byte %0000 
    .byte %0000
    .byte %0000

; dark dots
    .byte %0101
    .byte %0000
    .byte %1010
    .byte %0000
    .byte %0101
    .byte %0000
    .byte %1010
    .byte %0000

; gray dots
    .byte %0101
    .byte %1010
    .byte %0101
    .byte %1010
    .byte %0101
    .byte %1010
    .byte %0101
    .byte %1010

; bright dots
    .byte %1010
    .byte %1111
    .byte %0101
    .byte %1111
    .byte %1010
    .byte %1111
    .byte %0101
    .byte %1111

; vertical bar
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100

; v l
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100

; v dl
    .byte %0100
    .byte %0100
    .byte %1100
    .byte %0100
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100

; dv l
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1110
    .byte %0110
    .byte %0110
    .byte %0110

; dd l
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1110
    .byte %0110
    .byte %0110
    .byte %0110

; d dl
    .byte %0000
    .byte %0000
    .byte %1100
    .byte %0100
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100

; dd dl
    .byte %0110
    .byte %0110
    .byte %1110
    .byte %0110
    .byte %1110
    .byte %0110
    .byte %0110
    .byte %0110

; dv
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110

; dd dr
    .byte %0000
    .byte %0000
    .byte %1110
    .byte %0110
    .byte %1110
    .byte %0110
    .byte %0110
    .byte %0110

; du ddl
    .byte %0110
    .byte %0110
    .byte %1110
    .byte %0110
    .byte %1110
    .byte %0000
    .byte %0000
    .byte %0000

; du l
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1110
    .byte %0000
    .byte %0000
    .byte %0000

; u dl
    .byte %0100
    .byte %0100
    .byte %1100
    .byte %0100
    .byte %1100
    .byte %0000
    .byte %0000
    .byte %0000

; l d
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1100
    .byte %0100
    .byte %0100
    .byte %0100

; u r
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0111
    .byte %0000
    .byte %0000
    .byte %0000

; u h
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1111
    .byte %0000
    .byte %0000
    .byte %0000

; d h
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %0100
    .byte %0100
    .byte %0100

; v r
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0111
    .byte %0100
    .byte %0100
    .byte %0100

; v
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100

; v h
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1111
    .byte %0100
    .byte %0100
    .byte %0100

; v dr
    .byte %0100
    .byte %0100
    .byte %0111
    .byte %0100
    .byte %0111
    .byte %0100
    .byte %0100
    .byte %0100

; dh r
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0111
    .byte %0110
    .byte %0110
    .byte %0110

; du dr
    .byte %0110
    .byte %0110
    .byte %0111
    .byte %0110
    .byte %0111
    .byte %0000
    .byte %0000
    .byte %0000

; dd dr
    .byte %0000
    .byte %0000
    .byte %0111
    .byte %0110
    .byte %0111
    .byte %0110
    .byte %0110
    .byte %0110

; h du
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %1111
    .byte %0000
    .byte %0000
    .byte %0000

; h dd
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %0110
    .byte %0110

; dv dr
    .byte %0110
    .byte %0110
    .byte %0111
    .byte %0110
    .byte %0111
    .byte %0110
    .byte %0110
    .byte %0110

; dv
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %0000
    .byte %1111
    .byte %0000
    .byte %0000
    .byte %0000

; dv dh
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %0110
    .byte %0110

; dh u
    .byte %0100
    .byte %0100
    .byte %1111
    .byte %0000
    .byte %1111
    .byte %0000
    .byte %0000
    .byte %0000

; h du
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %0000
    .byte %0000
    .byte %0000

; dh d
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %0000
    .byte %1111
    .byte %0100
    .byte %0100
    .byte %0100

; h du
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %0110
    .byte %0110
    .byte %0110

; du r
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0111
    .byte %0000
    .byte %0000
    .byte %0000

; u dr
    .byte %0100
    .byte %0100
    .byte %0111
    .byte %0100
    .byte %0111
    .byte %0000
    .byte %0000
    .byte %0000

; d dr
    .byte %0000
    .byte %0000
    .byte %0111
    .byte %0100
    .byte %0111
    .byte %0100
    .byte %0100
    .byte %0100

; du r
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0111
    .byte %0110
    .byte %0110
    .byte %0110

; dv h
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %1111
    .byte %0110
    .byte %0110
    .byte %0110

; v dh
    .byte %0100
    .byte %0100
    .byte %1111
    .byte %0100
    .byte %1111
    .byte %0100
    .byte %0100
    .byte %0100

; l u
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %0100
    .byte %1100
    .byte %0000
    .byte %0000
    .byte %0000

; r d
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0111
    .byte %0100
    .byte %0100
    .byte %0100

; white block
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111

; white block bottom
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111

; white block left
    .byte %1100
    .byte %1100
    .byte %1100
    .byte %1100
    .byte %1100
    .byte %1100
    .byte %1100
    .byte %1100

; white block right
    .byte %0011
    .byte %0011
    .byte %0011
    .byte %0011
    .byte %0011
    .byte %0011
    .byte %0011
    .byte %0011

; white block top
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %1111
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000

; alpha
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0101
    .byte %1010
    .byte %1010
    .byte %0101
    .byte %0000
; beta
    .byte %0100
    .byte %1010
    .byte %1100
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %1000
    .byte %0000
;
    .byte %1110
    .byte %1010
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %0000
; pi
    .byte %0000
    .byte %0000
    .byte %1111
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0000
; sum
    .byte %1110
    .byte %1010
    .byte %1000
    .byte %0100
    .byte %1000
    .byte %1010
    .byte %1110
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0111
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000
;
    .byte %0000
    .byte %0101
    .byte %0101
    .byte %0101
    .byte %0110
    .byte %0100
    .byte %0100
    .byte %1000
;
    .byte %0000
    .byte %0101
    .byte %1010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0000
;
    .byte %1110
    .byte %0100
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0100
    .byte %1110
    .byte %0000
;
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000
; omega
    .byte %0110
    .byte %1001
    .byte %1001
    .byte %1001
    .byte %0110
    .byte %0110
    .byte %1001
    .byte %0000
;
    .byte %0110
    .byte %1000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1010
    .byte %1010
    .byte %0100
    .byte %0000
    .byte %0000
; O
    .byte %0110
    .byte %1010
    .byte %1010
    .byte %1110
    .byte %1010
    .byte %1010
    .byte %1100
    .byte %0000
;
    .byte %0011
    .byte %0100
    .byte %1000
    .byte %1111
    .byte %1000
    .byte %0100
    .byte %0011
    .byte %0000
;
    .byte %0110
    .byte %1001
    .byte %1001
    .byte %1001
    .byte %1001
    .byte %1001
    .byte %1001
    .byte %0000
;
    .byte %0000
    .byte %1111
    .byte %0000
    .byte %1111
    .byte %0000
    .byte %1111
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %1110
    .byte %0100
    .byte %0000
    .byte %1110
    .byte %0000
;
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %0000
    .byte %1110
    .byte %0000
;
    .byte %0010
    .byte %0100
    .byte %1000
    .byte %0100
    .byte %0010
    .byte %0000
    .byte %1110
    .byte %0000
;
    .byte %0100
    .byte %1010
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
    .byte %1000
;
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %1010
    .byte %0100
;
    .byte %0000
    .byte %0100
    .byte %0000
    .byte %1110
    .byte %0000
    .byte %0100
    .byte %0000
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0101
    .byte %1010
    .byte %0000
    .byte %0101
    .byte %1010
    .byte %0000
;
    .byte %0100
    .byte %1010
    .byte %0100
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %0100
    .byte %0000
    .byte %0000
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0100
    .byte %0000
    .byte %0000
    .byte %0000
;
    .byte %0011
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %0010
    .byte %1010
    .byte %0110
    .byte %0010
;
    .byte %1010
    .byte %0101
    .byte %0101
    .byte %0101
    .byte %0101
    .byte %0000
    .byte %0000
    .byte %0000
;
    .byte %0100
    .byte %1010
    .byte %0010
    .byte %0100
    .byte %1110
    .byte %0000
    .byte %0000
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0110
    .byte %0000
    .byte %0000
;
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
    .byte %0000
