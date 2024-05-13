#ifndef DESKTOP_H
#define DESKTOP_H

#define DESKTOP_WIDTH   (20 * 8)
#define DESKTOP_HEIGHT  (12 * 16 - MESSAGE_HEIGHT)
#define DESKTOP_BANK    1

extern struct obj * desktop;
extern struct window * focussed_window;

void start_desktop (void);
void desktop_loop (void);
void save_desktop_state (void);

#endif /* #ifndef DESKTOP_H */
