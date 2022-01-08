#ifndef KEYBOARD_H
#define KEYBOARD_H

extern char peek_key         (void);
extern char get_key          (void);
extern void unlog_key        (void);
extern void reset_log        (void);
extern char has_logged_keys  (void);
extern void start_playback   (void);
extern void stop_playback    (void);
extern void keyboard_init    (void);

#endif // #ifndef KEYBOARD_H
