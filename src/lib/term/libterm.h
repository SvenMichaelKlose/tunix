#ifndef LIBTERM_H
#define LIBTERM_H

#define TERM_SET_CURSOR         0x01
#define TERM_INSERT_LINE        0x02
#define TERM_BELL               0x07
#define TERM_LINE_FEED          0x0a
#define TERM_CARRIAGE_RETURN    0x0d
#define TERM_CLEAR_TO_EOL       0x18
#define TERM_ESCAPE             0x1b
#define TERM_ENABLE_ATTR        0x0b
#define TERM_DISABLE_ATTR       0x0c

#define TERM_ATTR_REVERSE       0x01
#define TERM_ATTR_CURSOR        0x10

extern void term_init (void);
extern void term_put (char);
extern void __fastcall__ term_puts (char *);
extern char term_get (void);

#endif /* #ifndef LIBTERM_H */
