#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgfx.h"
#include "obj.h"
#include "list.h"
#include "table.h"
#include "layout-ops.h"

#include "message.h"
#include "error.h"

void __fastcall__ layout_table (struct obj * t);

struct obj_ops table_ops = {
    draw_list,
    layout_table
};

struct obj *
make_table ()
{
    struct obj * table = OBJ(make_list (LIST_VERTICAL));
    table->ops = &table_ops;
    return table;
}

uchar __fastcall__
get_common_column_sizes (uchar * column_sizes, struct obj * row)
{
    struct obj * c = row->node.children;
    uchar i = 0;
    gsize h = 0;

    while (c) {
        if (i == MAX_TABLE_COLUMNS)
            break;

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
get_common_column_sizes_for_all_rows (uchar * column_sizes, struct obj * c)
{
    c = c->node.children;
    while (c) {
        get_common_column_sizes (column_sizes, c);
        c = c->node.next;
    }
}

gsize __fastcall__
layout_row (uchar * column_sizes, gsize h, struct obj * row)
{
    struct obj * c = row->node.children;
    uchar i = 0;
    gpos x = 0;

    while (c) {
        if (i == MAX_TABLE_COLUMNS)
            break;

        c->rect.x = x;
        c->rect.y = 0;
        c->rect.w = column_sizes[i];
        c->rect.h = h;
        x += column_sizes[i];
        c = c->node.next;
        i++;
    }

    row->rect.w = x;
    row->rect.h = h;

    return x;
}

void __fastcall__
layout_rows (uchar * column_sizes, struct obj * t)
{
    struct obj * row = t->node.children;
    gpos y = 0;
    gsize h;
    gsize w;

    /* Relocate and resize. */
    while (row) {
        h = get_common_column_sizes (column_sizes, row);
        w = layout_row (column_sizes, h, row);
        y += h;
        row = row->node.next;
    }

    t->rect.w = w;
    t->rect.h = y;
}

void __fastcall__
layout_table (struct obj * t)
{
    uchar * column_sizes = malloc (MAX_TABLE_COLUMNS);
    bzero (column_sizes, MAX_TABLE_COLUMNS);

    get_common_column_sizes_for_all_rows (column_sizes, t);
    layout_rows (column_sizes, t);

    free (column_sizes);
}
