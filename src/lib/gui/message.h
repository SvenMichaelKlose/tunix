#ifndef MESSAGE_H
#define MESSAGE_H

#define MESSAGE_HEIGHT  10

extern char message_buffer[64];

extern void __fastcall__ print_message (char *);
extern void __fastcall__ print_obj (struct obj *);

#endif /* #ifndef MESSAGE_H */
