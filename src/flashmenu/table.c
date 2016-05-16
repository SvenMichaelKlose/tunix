#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "libgfx.h"
#include "obj.h"
#include "list.h"
#include "table.h"
#include "layout-ops.h"

void __fastcall__ layout_table (struct obj * t);

struct obj_ops table_ops = {
    draw_table,
    layout_table
};

struct obj * __fastcall__
make_table (char cc65_bug_workaround)
{
    struct obj * table = OBJ(make_list (LIST_VERTICAL));
    table->ops = &table_ops;
    return table;
}

void __fastcall__
draw_table (void * _t)
{
    gfx_push_region ();
    set_obj_region (OBJ(_t));
    draw_obj_children (OBJ(_t));
    gfx_pop_region ();
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
set_common_column_sizes (uchar * column_sizes, gpos x, gpos y, uchar h, struct obj * row)
{
    uchar i = 0;
    struct obj * c = row->node.children;
    gsize w = 0;

    while (c) {
        if (i == MAX_TABLE_COLUMNS)
            break;

        c->rect.x = x;
        c->rect.y = y;
        c->rect.w = column_sizes[i];
        c->rect.h = h;
        x += column_sizes[i];
        w += column_sizes[i];
        c = c->node.next;
        i++;
    }

    return w;
}

void __fastcall__
relocate_and_resize (uchar * column_sizes, struct obj * t)
{
    struct obj * c = t->node.children;
    gpos x = 0;
    gpos y = 0;
    gsize w;
    gsize h;

    /* Relocate and resize. */
    while (c) {
        c->rect.x = x;
        c->rect.y = y;
        h = get_common_column_sizes (column_sizes, c);
        w = set_common_column_sizes (column_sizes, x, y, h, c);
        y += h;
        c = c->node.next;
    }

    t->rect.w = w;
    t->rect.h = y - t->rect.y;
}

void __fastcall__
layout_table (struct obj * t)
{
    uchar * column_sizes = malloc (MAX_TABLE_COLUMNS);
    bzero (column_sizes, MAX_TABLE_COLUMNS);

    get_common_column_sizes_for_all_rows (column_sizes, t);
    relocate_and_resize (column_sizes, t);

    free (column_sizes);
}
