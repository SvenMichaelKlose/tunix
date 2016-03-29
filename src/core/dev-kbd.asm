; Based on matrix table at http://sta.c64.org/cbm64kbdlay.html

CLR_HOME = 235
INS_DEL  = 255
CURSOR_LEFT = 254
CURSOR_RIGHT = 253
CURSOR_UP = 252
CURSOR_DOWN = 251
LEFT_SHIFT = 250
RIGHT_SHIFT = 249
POUND = 248
ESCAPE = 247
CTRL = 246
COMMODORE = 245
RUN_STOP = 244
F1 = 243
F2 = 242
F3 = 241
F4 = 240
F5 = 239
F6 = 238
F7 = 237
F8 = 236

RETURN  = 13

devkbd_map_normal:
    INS_DEL RETURN CURSOR_RIGHT F7 F1 F3 F5 CURSOR_DOWN
    "3wa4zse" LEFT_SHIFT
    "5rd6cftx"
    "7yg8bhuv"
    "9ij0mkon"
    "+pl-.:@,"
    POUND "*;" CLR_HOME RIGHT_SHIFT "=^/"
    "1" ESCAPE CTRL 2 " " COMMODORE "q" RUN_STOP

devkbd_map_normal:
    INS_DEL RETURN CURSOR_LEFT F8 F2 F4 F6 CURSOR_UP
    "#WA$ZSE" LEFT_SHIFT
    "%RD&CFTX"
    "'YG(BHUV"
    ")IJ0MKON"
    "+PL->[@<"
    POUND "*]" CLR_HOME RIGHT_SHIFT "=^?"
    "!" ESCAPE CTRL "\"" " " COMMODORE "Q" RUN_STOP
