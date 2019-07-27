#ifndef MAIN_H
#define MAIN_H

#define DESKTOP_BANK    1

void save_desktop_state (void);
void restart (void);
extern struct obj * desktop;
extern struct obj * focussed_window;

#endif /* #ifndef MAIN_H */
