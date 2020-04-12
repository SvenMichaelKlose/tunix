#include <stdlib.h>
#include <stdio.h>

#include "ultimem.h"

#include "obj.h"
#include "event.h"

#define EVENT_QUEUE_SIZE    8
#define EVENT_QUEUE_MASK    (EVENT_QUEUE_SIZE - 1)

struct queued_event {
    struct event * event;
    struct obj *   obj;
};

struct queued_event event_queue[EVENT_QUEUE_SIZE];
unsigned char event_queue_index = 0;

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
init_event_queue ()
{
    char i;

    for (i = 0; i < EVENT_QUEUE_SIZE; i++)
        event_queue[i].event = NULL;
}

void
send_queued_event ()
{
    char i;
    struct queued_event * q;

    for (i = 0; i < EVENT_QUEUE_SIZE; i++) {
        q = &event_queue[event_queue_index++ & EVENT_QUEUE_MASK];
        if (q->event) {
            send_event (q->obj, q->event);
            free (q->event);
            q->event = NULL;
            return;
        }
    }
}

char
free_event_queue_index ()
{
    char i;
    char i2;

    for (i = 0; i < EVENT_QUEUE_SIZE; i++) {
        i2 = event_queue_index++ & EVENT_QUEUE_MASK;
        if (!event_queue[i2].event)
            return i2;
    }

    return 0;
}

void
enqueue_event (struct obj * o, struct event * e)
{
    char idx;
    struct queued_event * q;

    while (!(idx = free_event_queue_index ()))
        send_queued_event ();

    q = &event_queue[idx];
    q->obj = o;
    q->event = e;
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
