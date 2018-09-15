#include <stdlib.h>
#include <stdio.h>

#include "obj.h"
#include "event.h"

char
event_handler_passthrough (struct obj * obj, struct event * event)
{
    return TRUE;
}

char __fastcall__
send_event (struct obj * o, struct event * e)
{
    return o->ops->event_handler (o, e);
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
