#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <cbm.h>
#include "g.h"

#include "ultimem.h"
#include "launch.h"
#include "bank-allocator.h"
#include "obj.h"
#include "button.h"
#include "layout-ops.h"
#include "list.h"
#include "table.h"
#include "window.h"
#include "message.h"
#include "file-window.h"
#include "main.h"

#define KEY_UP      145
#define KEY_DOWN    17
#define KEY_RETURN  13

struct cbm_dirent dirent;

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

    cbm_opendir (2, 8, path);

    while (1) {
        if (cbm_readdir (2, &dirent))
            break;
        if (dirent.type == CBM_T_HEADER || (dirent.type == CBM_T_DIR && (!strcmp (".", dirent.name) || !strcmp ("..", dirent.name))))
            continue;
        len++;
        d = malloc (sizeof (struct dirent));
        if (last_dirent)
            last_dirent->next = d;
        else
            first_dirent = d; last_dirent = d;

        d->size = dirent.size;
        d->type = dirent.type;
        memcpy (&d->name, &dirent.name, 17);
    }
    cbm_closedir (2);

    content->files = first_dirent;
    content->len = len;
}

void
file_window_invert_position (struct file_window_content * content)
{
    unsigned visible_lines = content->obj.rect.h / 8 - 1;
    unsigned wpos = content->pos - (content->pos % visible_lines);
    unsigned y = (content->pos - wpos) * 8;

    if (y > content->obj.rect.h || !content->len)
        return;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region ((struct obj *) content);
    gfx_set_pattern (pattern_solid);
    gfx_set_pencil_mode (PENCIL_MODE_XOR);
    gfx_draw_box (0, y, content->obj.rect.w, 8);
    gfx_pop_context ();
}

struct dirent * __fastcall__
file_window_get_file_by_index (struct file_window_content * content, unsigned x)
{
    struct dirent * d = content->files;

    while (d && x--)
        d = d->next;

    return d;
}

void __fastcall__
file_window_draw_list (struct obj * w)
{
    struct window * win = (struct window *) w;
    struct file_window_content * content = (struct file_window_content *) w;
    unsigned visible_lines = content->obj.rect.h / 8 - 1;
    char xofs = 1;
    char c;
    char size[8];
    struct dirent * d = content->files;
    unsigned y = 0;
    unsigned wpos = content->pos - (content->pos % visible_lines);
    unsigned rpos = wpos;
    uchar i;
    uchar t;

    d = file_window_get_file_by_index (content, wpos);

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
        switch (d->type) {
            case CBM_T_SEQ:
                t = 's'; break;
            case CBM_T_PRG:
                t = 'p'; break;
            case CBM_T_USR:
                t = 'u'; break;
            case CBM_T_REL:
                t = 'r'; break;
            case CBM_T_VRP:
                t = 'v'; break;
            case CBM_T_DEL:
                t = 'x'; break;
            case CBM_T_CBM:
                t = 'c'; break;
            case CBM_T_DIR:
                t = 'd'; break;
            case CBM_T_LNK:
                t = 'l'; break;
            case CBM_T_HEADER:
                t = 'h'; break;
            default:
                t = 'o';
        }
        gfx_putchar (t);
        gfx_putchar (32);

        /* Print file size. */
        gfx_set_position (xofs + 6, y);
        memset (size, 32, sizeof (size));
        sprintf (size, "%U", (unsigned int) d->size);
        size[strlen (size)] = 32;
        for (i = 0; i < 5; i++)
            gfx_putchar (size[i]);
        gfx_putchar (32);

        /* Print file name. */
        gfx_set_font ((void *) 0x8000, 0);
        gfx_set_font_compression (0);
        gfx_set_position (xofs + 28, y);
        for (i = 0; i < 16; i++)
            if (c = d->name[i]) {
                gfx_putchar ((c - 64) & 127);
            } else
                break;

        d = d->next;
next:
        y += 8;
        rpos++;
    }

    if (!content->len) {
        gfx_set_font (charset_4x8, 2);
        gfx_draw_text (1, 1, "No files.");
    }
    gfx_pop_context ();
}


void __fastcall__
file_window_draw_content (struct obj * w)
{
    struct file_window_content * content = (struct file_window_content *) w;

    gfx_push_context ();
    gfx_reset_region ();
    set_obj_region (w);
    file_window_draw_list (w);
    file_window_invert_position (content);
    gfx_pop_context ();
}

typedef void __fastcall__ (*launch_t) (unsigned start, unsigned size);

void
file_window_launch_program (struct dirent * d)
{
    unsigned start;
    unsigned read_bytes;
    unsigned size;
    uchar oldblk5 = *ULTIMEM_BLK5RAM;
    launch_t launcher = (void *) 0x9800;

    print_message ("Loading...");

    cbm_open (2, 8, 0, d->name);
    cbm_read (2, &start, 2);

    *ULTIMEM_BLK5RAM = 8;
    while (1) {
        if (cbm_k_readst ())    /* cbm_read() returns 1 on end-of-file. */
            break;
        read_bytes = cbm_read (2, (void *) 0xa000, 0x2000);
        if (read_bytes == -1) {
            print_message ("Read error!");
            goto error;
        }
        size += read_bytes;
        *ULTIMEM_BLK5RAM = *ULTIMEM_BLK5RAM + 1;

        sprintf (message_buffer, "%U read...", size);
        print_message (message_buffer);
    }

    sprintf (message_buffer, "Launching at %U...", start);
    print_message (message_buffer);
error:
    cbm_close (2);
    *ULTIMEM_BLK5RAM = oldblk5;
    memcpy (launcher, launch, 256);
    launcher (start, size);
}

void
file_window_enter_directory (struct file_window_content * content, struct dirent * d)
{
    sprintf (message_buffer, "cd/%S/", d->name);
    print_message (message_buffer);
    cbm_open (15, 8, 15, message_buffer);
    cbm_read (15, message_buffer, 63);
    message_buffer[63] = 0;
    print_message (message_buffer);
    cbm_close (15);
    file_window_free_files (content);
    file_window_read_directory (content, "$");
    file_window_draw_content ((struct obj *) content);
}

void
file_window_launch (struct file_window_content * content, struct dirent * d)
{
    switch (d->type) {
        case CBM_T_PRG:
            file_window_launch_program (d);
            break;

        case CBM_T_DIR:
            file_window_enter_directory (content, d);
            break;

        default:
            print_message ("Don't know how to open this.");
    }
}

char
file_window_event_handler (struct obj * o, struct event * e)
{
    struct file_window_content * content = (struct file_window_content *) o->node.children;
    int visible_lines = content->obj.rect.h / 8 - 1;
    unsigned wpos = content->pos - (content->pos % visible_lines);

    file_window_invert_position (content);

    switch (e->data_char) {
        case KEY_RETURN:
            file_window_launch (content, file_window_get_file_by_index (content, content->pos));
            break;

        case KEY_UP:
            if (!content->pos)
                goto done;
            content->pos--;
            if (content->pos < wpos)
                goto new_page;
            break;

        case KEY_DOWN:
            if (content->pos > content->len)
                goto done;
            content->pos++;
            if (content->pos > wpos + visible_lines - 1)
                goto new_page;
            break;
    }

done:
    file_window_invert_position (content);
    return FALSE;

new_page:
    file_window_draw_content ((struct obj *) content);
    return FALSE;
}

struct obj_ops obj_ops_file_window_content = {
    file_window_draw_content,
    obj_noop,
    obj_noop,
    event_handler_passthrough
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
    struct file_window_content * content = (struct file_window_content *) make_file_window_content ();
	struct window * win = make_window (title, (struct obj *) content, file_window_event_handler);

	set_obj_position_and_size (OBJ(win), x, y, w, h);

    file_window_free_files (content);
    file_window_read_directory (content, "$");

    return OBJ(win);
}
