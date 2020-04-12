#include <stdlib.h>
#include <stdio.h>

#include "ultimem.h"

#include "obj.h"
#include "event.h"

struct queued_event {
    struct queued_event *  next;
    struct event *         event;
    struct obj *           obj;
};

struct queued_event * first_event = NULL;

void __fastcall__
event_handler_passthrough (struct obj * obj, struct event * event)
{
}

void __fastcall__
send_event (struct obj * o, struct event * e)
{
    unsigned short oldbank = *ULTIMEM_BLK1;

    *ULTIMEM_BLK1 = o->ops->event_handler_bank;
    o->ops->event_handler (o, e);
    *ULTIMEM_BLK1 = oldbank;
}

void
send_queued_event ()
{
    struct queued_event * q = first_event;
    struct event * e;

    if (!q)
        return;

    first_event = q->next;

    send_event (q->obj, q->event);

    free (e);
    free (q);
}

void
enqueue_event (struct obj * o, struct event * e)
{
    struct queued_event * p;
    struct queued_event * q = malloc (sizeof (struct queued_event));
    q->next = NULL;
    q->obj = o;
    q->event = e;

    if (first_event) {
        for (p = first_event; p->next; p = p->next);
        p->next = q;
    } else
        first_event = q;
}

/*
struct obj * __fastcall__
find_obj_at (struct obj * o, struct event * ev, gpos x, gpos y)
{
    return NULL;
}

void __fastcall__
dispatch_event (struct obj * o, struct event * ev, gpos x, gpos y)
{
    struct obj * found;

    while (found = find_obj_at (o, ev, x, y)) {
        x -= found->rect.x;
        y -= found->rect.y;
        o = found;
    }

    while (o && send_event (o, ev, x, y)) {
        x += o->rect.x;
        y += o->rect.y;
        o = o->node.parent;
    }
}
*/
