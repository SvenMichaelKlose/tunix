screen_columns  = 20
screen_rows     = 11
screen_width    = @(* 8 screen_columns)
screen_height   = @(* 16 screen_rows)

screen  = $1e00
colors  = $9600
charset = $1000

    data
    org 0

s:      0 0
d:      0 0
c:      0 0
p:      0 0

sa:     0 0
sp:     0 0
srx:    0

tmp:    0
tmp2:   0
tmp3:   0

; Drawing primitives

scr:    0 0     ; Current screen address.

xpos:   0       ; X position in pixles.
ypos:   0       ; Y position in pixles.
width:  0       ; Width in pixels.
height: 0       ; Height in pixels.
masks:  0       ; Source mask.
maskd:  0       ; Destination mask.

; Utils

font_compression:       0

; Text

font:   0
do_compress_font_gaps:  0
font_space_size:        0
pattern:    0 0

xcpos:  0

; Temp
xright: 0

    end
