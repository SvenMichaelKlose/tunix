#ifndef DESKTOP_H
#define DESKTOP_H

#define DESKTOP_BANK    1

extern struct obj * desktop;
extern struct obj * focussed_window;

void start_desktop (void);
void desktop_loop (void);
void save_desktop_state (void);

#endif /* #ifndef DESKTOP_H */
