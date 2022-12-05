#ifndef ALLOC_H
#define ALLOC_H

extern void init_alloc ();
extern char alloc_bank ();
extern char __fastcall__ free_bank (char num);

#endif // #ifndef ALLOC_H
