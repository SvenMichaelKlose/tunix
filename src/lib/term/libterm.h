#ifndef LIBTERM_H
#define LIBTERM_H

#define TERM_CLEAR_TO_EOL   18
#define TERM_ESC            18
#define TERM_SET_CURSOR     18

extern void term_init (void);
extern void term_put (char);
extern void __fastcall__ term_puts (char *);

#endif /* #ifndef LIBTERM_H */
