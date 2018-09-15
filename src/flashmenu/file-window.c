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

#define KEY_UP      145
#define KEY_DOWN    17

char name[21];

void
file_window_free_files (struct file_window * fw)
{
    struct dirent * d = fw->files;
    struct dirent * n;

    while (d) {
        n = d->next;
        free (d);
        d = n;
    }
}

void
file_window_read_directory (struct file_window * fw, char * path)
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

    fw->files = first_dirent;
    fw->len = len;
}

void
file_window_invert_position (struct file_window * fw)
{
    char yofs = fw->win.flags & W_FULLSCREEN ? 11 : 12;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region ((struct obj *) fw);
    gfx_set_pattern (pattern_solid);
    gfx_set_pencil_mode (PENCIL_MODE_XOR);
    gfx_draw_box (0, yofs + (fw->pos - fw->wpos) * 8, fw->win.obj.rect.w, 8);
    gfx_pop_context ();
}

void __fastcall__
file_window_draw_list (struct obj * w)
{
    struct window * win = (struct window *) w;
    struct file_window * fw = (struct file_window *) w;
    char xofs = win->flags & W_FULLSCREEN ? 1 : 2;
    char yofs = win->flags & W_FULLSCREEN ? 11 : 12;
    char c;
    char size[8];
    struct dirent * d = fw->files;
    unsigned rpos = fw->wpos;
    unsigned y = yofs;
    int i = 0;

    gfx_push_context ();
    while (1) {
        if (y > win->obj.rect.h)
            break;

        /* Clear entry. */
        gfx_set_pattern (pattern_empty);
        gfx_draw_box (0, y, win->obj.rect.w, 8);

        if (rpos >= fw->len)
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
    struct window * win = (struct window *) w;
    struct file_window * fw = (struct file_window *) w;

    file_window_free_files (fw);
    file_window_read_directory (fw, "$");

    window_ops.draw (w);
    file_window_draw_list (w);
    file_window_invert_position (fw);
}

char
file_window_event_handler (struct obj * o, struct event * e)
{
    struct file_window * fw = (struct file_window *) o;

    file_window_invert_position (fw);

    switch (e->data_char) {
        case KEY_UP:
            if (!fw->pos)
                goto done;
            fw->pos--;
            break;

        case KEY_DOWN:
            if (fw->pos == fw->len - 1)
                goto done;
            fw->pos++;
            break;

        default:
            goto done;
    }

done:
    file_window_invert_position (fw);
    return FALSE;
}

struct obj *__fastcall__
make_file_window (char * title, gpos x, gpos y, gpos w, gpos h)
{
	struct window * win = make_window (title);
	struct obj_ops * ops = malloc (sizeof (struct obj_ops));
    struct file_window * fw = malloc (sizeof (struct file_window));

    win->flags |= W_FULLSCREEN;

    memcpy (fw, win, sizeof (struct window));
    free (win);
    fw->files = NULL;

    copy_obj_ops (ops, &window_ops);
    ops->draw = file_window_draw;
    set_obj_ops (OBJ(fw), ops);
    fw->win.obj.ops->event_handler = file_window_event_handler;

	set_obj_position_and_size (OBJ(fw), x, y, w, h);

    return OBJ(fw);
}
