#ifndef KEYBOARD_H
#define KEYBOARD_H

extern char peek_key        (void);
extern char get_key         (void);
extern void unlog_key       (void);
extern void keyboard_init   (void);
extern void start_playback  (void);
extern void stop_playback   (void);

#endif // #ifndef KEYBOARD_H
