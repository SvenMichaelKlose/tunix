#include <stdbool.h>
#include <string.h>
#include <stdio.h>
#include <conio.h>

typedef unsigned char uchar;

#include <ultimem/ultimem.h>
#define BLK3_START  ((void *) 0x6000)
#define BLK4_START  ((void *) 0x8000)

#define NUM_BOOT_BANKS      6
#define REAL_NUM_BOOT_BANKS 8
#define MAX_BANK_NAME       16

#define SWITCH1_PRESSED() \
  (*ULTIMEM_CONTROL & ULTIMEM_CTRL_SWITCH1)

char detected[NUM_BOOT_BANKS];

char boot_bank_names[REAL_NUM_BOOT_BANKS][MAX_BANK_NAME];

#pragma code-name (push, "TRAMPOLINE")
void __fastcall__
boot (char bank)
{
    *ULTIMEM_BLK5 = bank;
    ((void (*)(void))(*(unsigned *) 0xa000))();
}
#pragma code-name (pop)

// Draw a rectangle using PETSCII graphics.
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

// Check if all bytes of a bank have value 0xff.
bool
is_bank_empty (uchar bank)
{
    char * p = BLK3_START;
    *ULTIMEM_CONFIG2 = 0xdf; // BLK3 ROM
    *ULTIMEM_BLK3 = bank;
    do {
        if (*p++ != 0xff)
            return false;
    } while (p != BLK4_START);
    return true;
}

// Erase bank using Flash ROM commands.
void
erase_bank (uchar bank)
{
    (void) bank;
}

// Read bank to memory.
void
read_bank (char * to, uchar bank)
{
    (void) to;
    (void) bank;
}

// Burn loaded image onto bank.
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

    // Draw panel for bank selection.
    y = 4;
    draw_panel (0, y, 21, NUM_BOOT_BANKS + 4, "Boot ROM editor");

    // Print option to boot BASIC.
    y += 2;
    cputsxy (2, y++, "0:BASIC");

    // Detect and print bank names.
    for (i = 0; i < NUM_BOOT_BANKS; i++) {
        cputcxy (2, y++, '0' + 1 + i);
        cputs (":");
        if (is_bank_empty (i))
            cputs ("(empty)");
        else if (!boot_bank_names[i])
            cputs ("unnamed");
        else
            for (j = 0; j < 16; j++) {
                c = boot_bank_names[i][j];
                cputc (c ? c : ' ');
            }
    }

    // Print command selection.
    y += 3;
    cputsxy (1, y++, "[Boot] [Kill] [Burn]  [Name] [View] [Quit]");
}

void
main (void)
{
    memset (boot_bank_names, 0, sizeof (boot_bank_names));
    clrscr ();
    *ULTIMEM_CONTROL = ULTIMEM_CTRL_LED;
    menu ();
    while (1);
    boot (SWITCH1_PRESSED() ? 1 : 127);
}
