#ifndef EVENT_H
#define EVENT_H

#define NUM_EVENT_HANDLERS  32

#define EVT_KEYPRESS    1
#define EVT_CLICK       2

struct event {
    char type;
};

char __fastcall__ send_event (struct obj * o, struct event * ev, gpos x, gpos y);
void __fastcall__ dispatch_event (struct obj * o, struct event * ev, gpos x, gpos y);

#endif /* #ifndef EVENT_H */
