#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <gfx/libgfx.h>
#include <gui/obj.h>
#include <gui/event.h>
#include <gui/layout-ops.h>
#include <gui/list.h>
#include <gui/table.h>
#include <gui/message.h>
#include <gui/error.h>

void __fastcall__ layout_table (struct obj *);
void __fastcall__ layout_table_center (struct obj *);

struct obj_ops table_ops = {
    draw_list,
    layout_table,
    obj_noop
};

struct obj_ops table_ops_center = {
    draw_list,
    layout_table_center,
    obj_noop,
    event_handler_passthrough
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

        set_obj_position_and_size (c, x, 0, column_sizes[i], h);
        x += column_sizes[i];
        c = c->node.next;
        i++;
    }

    set_obj_size (row, x, h);

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
        row->rect.y = y;
        h = get_common_column_sizes (column_sizes, row);
        w = layout_row (column_sizes, h, row);
        y += h;
        row = row->node.next;
    }

    set_obj_size (t, w, y);
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

void __fastcall__
layout_table_center (struct obj * obj)
{
    layout_table (obj);
    layout_center (obj);
}
