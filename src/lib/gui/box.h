#ifndef BOX_H
#define BOX_H

extern struct obj_ops box_ops;

struct box {
    struct obj obj;
    char * pattern;
};

struct box * make_box (char * pattern);
void __fastcall__ draw_box (struct obj * o);


#endif /* #ifndef BOX_H */
