#ifndef INPUTLINE_H
#define INPUTLINE_H

#define INPUTLINE(x)   ((struct inputline *) x)

extern struct obj *     inputline;
extern struct obj_ops   inputline_ops;

struct inputline {
    struct obj  obj;
};

extern struct inputline * __fastcall__ make_inputline (char * text);
extern void inputline_close (void);
extern void __fastcall__ draw_inputline (struct obj *);
extern void __fastcall__ inputline_input (char);

#endif /* #ifndef INPUTLINE_H */
