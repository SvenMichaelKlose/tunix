#include <stdlib.h>
#include <stdio.h>

#include "obj.h"
#include "event.h"

typedef char (*event_handler_t) (struct obj *, struct event *, gpos x, gpos y);

event_handler_t event_handlers[NUM_EVENT_HANDLERS];

struct obj * __fastcall__
find_obj_at (struct obj * o, struct event * ev, gpos x, gpos y)
{
    return NULL;
}

char __fastcall__
send_event (struct obj * o, struct event * ev, gpos x, gpos y)
{
    return event_handlers[o->event_handler] (o, ev, x, y);
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
