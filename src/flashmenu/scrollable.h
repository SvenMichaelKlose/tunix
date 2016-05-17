#ifndef SCROLLABLE_H
#define SCROLLABLE_H

extern struct obj_ops scrollable_ops;

struct scrollable {
    struct obj obj;
    char bank;
};

struct scrollable * make_scrollable (void);

#endif /* #ifndef SCROLLABLE_H */
