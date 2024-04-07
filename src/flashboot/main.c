#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <conio.h>

typedef unsigned char uchar;

#include <lib/ultimem/ultimem.h>
#define BLK3_START  ((void *) 0x6000)
#define BLK4_START  ((void *) 0x8000)

#define NUM_BOOT_BANKS  6

#define SWITCH1_PRESSED() \
  (*ULTIMEM_CONTROL & ULTIMEM_CTRL_SWITCH1)

char detected[NUM_BOOT_BANKS];
char names[8 * 16];

#pragma code-name (push, "TRAMPOLINE")
void __fastcall__
boot (char bank)
{
    *ULTIMEM_BLK5 = bank;
    ((void (*)(void))(*(unsigned *) 0xa000))();
}
#pragma code-name (pop)

void
draw_panel (uchar x, uchar y,
            uchar w, uchar h,
            char * title)
{
    cputcxy (x, y, 0xf0);
    cputcxy (x + w, y, 0xee);
    cputcxy (x, y + h, 0xed);
    cputcxy (x + w, y + h, 0xfd);
    chlinexy (x + 1, y, w - 1);
    chlinexy (x + 1, y + h, w - 1);
    cvlinexy (x, y + 1, h - 1);
    cvlinexy (x + w, y + 1, h - 1);
    cputsxy (x + 1, y, title);
}

bool
bank_is_empty (uchar bank)
{
    char * p;
    *ULTIMEM_BLK3 = bank;
    p = BLK3_START;
    do {
        if (*p++ != 0xff)
            return false;
    } while (p != BLK4_START);
    return true;
}

bool
detect_bank (uchar bank)
{
    (void) bank;
    return true;
}

void
detect_banks (void)
{
    char i;
    for (i = 0; i < NUM_BOOT_BANKS; i++)
        detect_bank (i);
}

void
erase_bank (uchar bank)
{
    (void) bank;
}

void
read_bank (char * to, uchar bank)
{
    (void) to;
    (void) bank;
}

void
burn_bank (uchar bank, char * data)
{
    (void) bank;
    (void) data;
}

void
move_bank (uchar from, uchar to)
{
    (void) from;
    (void) to;
}

void
menu (void)
{
    char c, i, j, y;

    y = 0;
    draw_panel (0, y, 21, NUM_BOOT_BANKS + 3, "Select a boot ROM:");
    y += 2;
    cputsxy (2, y++, "0:BASIC");
    for (i = 0; i < NUM_BOOT_BANKS; i++) {
        cputcxy (2, y++, '0' + 1 + i);
        cputs (":");
        for (j = 0; j < 16; j++) {
            c = names[i * 16 + j];
            cputc (c ? c : ' ');
        }
    }
    y += 2;
    cputsxy (1, y++, "K: +3 +8 +16 +24");
    cputsxy (1, y++, "[Boot] [Save] [Load]  [Burn] [Move] [Name]  [Dump]");
}

void
main (void)
{
    memset (names, 32, sizeof (names));
    strcpy (names, "TUNIX");
    strcpy (&names[16], "ULTIBOOT");
    strcpy (&names[32], "VFORTH");
    clrscr ();
    *ULTIMEM_CONTROL = ULTIMEM_CTRL_LED;
    menu ();
    while (1);
    boot (SWITCH1_PRESSED() ? 1 : 127);
}
