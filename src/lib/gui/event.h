#ifndef EVENT_H
#define EVENT_H

#include "obj.h"

#define NUM_EVENT_HANDLERS  32

#define EVT_KEYPRESS    1
#define EVT_CLICK       2

struct event {
    char type;
    union {
        char data_char;
        int  data_int;
    };
};

typedef void __fastcall__ (*event_handler_t) (struct obj *, struct event *);

extern void __fastcall__ event_handler_passthrough (struct obj *, struct event *);

extern void __fastcall__ send_event (struct obj *, struct event *);
extern void __fastcall__ enqueue_event (struct obj *, struct event *);
extern void __fastcall__ send_queued_event (void);

//extern void __fastcall__ dispatch_event (struct obj *, struct event *, gpos x, gpos y);

#endif /* #ifndef EVENT_H */
