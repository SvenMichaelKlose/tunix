#ifndef LIBKEYBOARD_H
#define LIBKEYBOARD_H

///////////////
// KEY CODES //
///////////////

#define TTY_ENTER           13
#define TTY_ESCAPE          0xf4
#define TTY_CURSOR_UP       5
#define TTY_CURSOR_DOWN     20
#define TTY_CURSOR_LEFT     19
#define TTY_CURSOR_RIGHT    4
#define TTY_BACKSPACE       8

extern char keyboard_get (void);

#endif /* #ifndef LIBKEYBOARD_H */
