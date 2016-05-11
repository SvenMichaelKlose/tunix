#ifndef TABLE_H
#define TABLE_H

#include "obj.h"

#define MAX_TABLE_COLUMNS   32

extern struct obj_ops table_ops;

extern struct obj * __fastcall__ make_table (gpos x, gpos y, gsize w, gsize h);
extern void __fastcall__ draw_table (void *);

#endif /* #ifndef TABLE_H */
