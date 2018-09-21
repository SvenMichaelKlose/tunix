#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "bank-allocator.h"
#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "file-window.h"
#include "cbm.h"
#include "main.h"

#define KEY_UP      145
#define KEY_DOWN    17

char name[21];

void
file_window_free_files (struct file_window_content * content)
{
    struct dirent * d = content->files;
    struct dirent * n;

    while (d) {
        n = d->next;
        free (d);
        d = n;
    }
}

void
file_window_read_directory (struct file_window_content * content, char * path)
{
    struct dirent * first_dirent = NULL;
    struct dirent * last_dirent = NULL;
    struct dirent * d;
    unsigned len = 0;

    cbm_opendir (path, 8);

    while (1) {
        cbm_readdir (name);
        if (!name[0])
            break;
        len++;
        d = malloc (sizeof (struct dirent));
        if (last_dirent)
            last_dirent->next = d;
        else
            first_dirent = d;
        last_dirent = d;
        memcpy (d, &name, 21);
    }
    cbm_closedir ();

    content->files = first_dirent;
    content->len = len;
}

void
file_window_invert_position (struct file_window_content * content)
{
    unsigned y = (content->pos - content->wpos) * 8;

    if (y > content->obj.rect.h)
        return;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region ((struct obj *) content);
    gfx_set_pattern (pattern_solid);
    gfx_set_pencil_mode (PENCIL_MODE_XOR);
    gfx_draw_box (0, y, content->obj.rect.w, 8);
    gfx_pop_context ();
}

void __fastcall__
file_window_draw_list (struct obj * w)
{
    struct window * win = (struct window *) w;
    struct file_window_content * content = (struct file_window_content *) w;
    char xofs = 1;
    char c;
    char size[8];
    struct dirent * d = content->files;
    unsigned y = 0;
    unsigned rpos = content->wpos;
    unsigned wpos = rpos;
    uchar i;

    while (d && wpos--)
        d = d->next;

    gfx_push_context ();
    while (1) {
        if (y > w->rect.h)
            break;

        /* Clear entry. */
        gfx_set_pattern (pattern_empty);
        gfx_draw_box (0, y, w->rect.w, 8);

        if (rpos >= content->len)
            goto next;
        if (!d || !d->name[0])
            goto next;

        /* Print file type. */
        gfx_set_font (charset_4x8, 0);
        gfx_set_font_compression (1);
        gfx_set_position (xofs, y);
        gfx_putchar (d->type);
        gfx_putchar (32);

        /* Print file size. */
        gfx_set_position (xofs + 6, y);
        memset (size, 32, sizeof (size));
        sprintf (size, "%u", (unsigned int) d->size);
        size[strlen (size)] = 32;
        for (i = 0; i < 5; i++)
            gfx_putchar (size[i]);
        gfx_putchar (32);

        /* Print file name. */
        gfx_set_font ((void *) 0x8000, 0);
        gfx_set_font_compression (0);
        gfx_set_position (xofs + 28, y);
        for (i = 0; i < 16; i++)
            if (c = d->name[i])
                gfx_putchar (c + 192);
            else
                break;

        d = d->next;
next:
        y += 8;
        rpos++;
    }
    gfx_pop_context ();
}


void __fastcall__
file_window_draw (struct obj * w)
{
    struct file_window_content * content = (struct file_window_content *) w;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region (w);
    file_window_free_files (content);
    file_window_read_directory (content, "$");
    file_window_draw_list (w);
    file_window_invert_position (content);
    gfx_pop_context ();
}

char
file_window_event_handler (struct obj * o, struct event * e)
{
    struct file_window_content * content = (struct file_window_content *) o;
    int visible_lines = o->rect.h / 8 - 1;

    file_window_invert_position (content);

    switch (e->data_char) {
        case KEY_UP:
            if (!content->pos)
                goto done;
            content->pos--;
            if (content->pos < content->wpos) {
                content->wpos -= visible_lines;
                goto new_page;
            }
            break;

        case KEY_DOWN:
            if (content->pos == content->len - 1)
                goto done;
            content->pos++;
            if (content->pos > content->wpos + visible_lines) {
                content->wpos += visible_lines;
                goto new_page;
            }
            break;
    }

done:
    file_window_invert_position (content);
    return FALSE;

new_page:
    file_window_draw (o);
    return FALSE;
}

struct obj_ops obj_ops_file_window_content = {
    file_window_draw,
    obj_noop,
    obj_noop,
    file_window_event_handler
};

struct obj *
make_file_window_content ()
{
	struct obj * obj =  alloc_obj (sizeof (struct obj), &obj_ops_file_window_content);
    struct file_window_content * content = malloc (sizeof (struct file_window_content));

    memcpy (content, obj, sizeof (struct obj));
    free (obj);
    content->files = NULL;

    return OBJ(content);
}

struct obj * __fastcall__
make_file_window (char * title, gpos x, gpos y, gpos w, gpos h)
{
    struct obj * content = make_file_window_content ();
	struct window * win = make_window (title, content);

    focussed_window = content;
    win->flags |= W_FULLSCREEN;
	set_obj_position_and_size (OBJ(win), x, y, w, h);

    return OBJ(win);
}
