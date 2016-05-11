#include <stdlib.h>
#include <string.h>

#include "libgfx.h"
#include "obj.h"
#include "list.h"
#include "table.h"
#include "error.h"
#include "layout-ops.h"

void __fastcall__ layout_table (struct obj * t);

struct obj_ops table_ops = {
    draw_table,
    layout_table
};

struct obj * __fastcall__
make_table (gpos x, gpos y, gsize w, gsize h)
{
    struct obj * table = OBJ(make_list (x, y, w, h, LIST_VERTICAL));
    table->ops = &table_ops;
    return table;
}

void __fastcall__
draw_table (void * _t)
{
    draw_obj_children (OBJ(_t));
}

uchar __fastcall__
get_common_column_sizes (uchar * column_sizes, struct obj * row)
{
    struct obj * c = row->node.children;
    uchar i = 0;
    uchar h = 0;

    while (c) {
        layout_obj (c);
        if (column_sizes[i] < c->rect.w)
            column_sizes[i] = c->rect.w;
        if (h < c->rect.h)
            h = c->rect.h;
        c = c->node.next;
        i++;
    }

    return h;
}

void __fastcall__
set_common_column_sizes (uchar * column_sizes, gpos x, gpos y, uchar h, struct obj * row)
{
    uchar i = 0;
    struct obj * c = row->node.children;
    char msg[32];

    while (c) {
        c->rect.x = x;
        c->rect.y = y;
        c->rect.w = column_sizes[i];
        c->rect.h = h;
        x += column_sizes[i];
        c = c->node.next;
        i++;
    }
}

void __fastcall__
layout_table (struct obj * t)
{
    uchar * column_sizes = malloc (MAX_TABLE_COLUMNS);
    gpos x = t->rect.x;
    gpos y = t->rect.y;
    uchar h;
    struct obj * c;

    /* Get common column sizes. */
    bzero (column_sizes, MAX_TABLE_COLUMNS);
    c = t->node.children;
    while (c) {
        get_common_column_sizes (column_sizes, c);
        c = c->node.next;
    }

    /* Relocate and resize. */
    c = t->node.children;
    while (c) {
        c->rect.x = x;
        c->rect.y = y;
        h = get_common_column_sizes (column_sizes, c);
        set_common_column_sizes (column_sizes, x, y, h, c);
        y += h;
        c = c->node.next;
    }

end:
    free (column_sizes);
}
