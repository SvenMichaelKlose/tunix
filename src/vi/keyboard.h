#ifndef KEYBOARD_H
#define KEYBOARD_H

extern char is_playback;

extern char peek_key       (void);
extern char get_key        (void);
extern void unlog_key      (void);
extern void keyboard_init  (void);

#endif // #ifndef KEYBOARD_H
