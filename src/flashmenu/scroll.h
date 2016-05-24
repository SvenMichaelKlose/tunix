#ifndef SCROLL_H
#define SCROLL_H

extern struct obj_ops scroll_ops;

struct scroll {
    struct obj obj;
    char bank;
};

struct scroll * make_scroll (void);

#endif /* #ifndef SCROLL_H */
