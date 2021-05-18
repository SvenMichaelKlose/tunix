#ifndef ULTIMEM_H
#define ULTIMEM_H

/*
 * For documentation on UltiMem programming see:
 * http://sleepingelephant.com/denial/wiki/index.php?title=UltiMem
 */

#define ULTIMEM_CONTROL         ((char *) 0x9ff0)
#define ULTIMEM_CTRL_LED        1
#define ULTIMEM_CTRL_SWITCH0    2       /* Middle switch. 0=pressed. */
#define ULTIMEM_CTRL_SWITCH1    2       /* Left-most switch. 0=pressed. */
#define ULTIMEM_CTRL_RESET      64      /* Soft-reset. Keep registers. */
#define ULTIMEM_CTRL_HIDE       128     /* Hide registers. */

#define ULTIMEM_CONFIG1     ((char *) 0x9ff1)   /* RAM123 (bits 0/1), IO2 (bits 2/3), IO3 (...) */
#define ULTIMEM_CONFIG2     ((char *) 0x9ff2)   /* BLK1, BLK2, BLK3, BLK5 */
#define ULTIMEM_CFG_ROM     1
#define ULTIMEM_CFG_RO_RAM  2
#define ULTIMEM_CFG_RW_RAM  3

#define ULTIMEM_ID          ((unsigned short *) 0x9ff3) /* Bits 0-3: ID, 4-7: Manufacturer */
#define ULTIMEM_ID_ULTIMEM  1
#define ULTIMEM_ID_VICMIDI  2

/* Bank numbers. */
#define ULTIMEM_RAM         ((unsigned short *) 0x9ff4)
#define ULTIMEM_IO          ((unsigned short *) 0x9ff6) /* IO2 and IO3. */
#define ULTIMEM_BLK1        ((unsigned short *) 0x9ff8)
#define ULTIMEM_BLK2        ((unsigned short *) 0x9ffa)
#define ULTIMEM_BLK3        ((unsigned short *) 0x9ffc)
#define ULTIMEM_BLK5        ((unsigned short *) 0x9ffe)

/* Unhide registers. Returns an ID. */
extern char ultimem_unhide (void);
#define ULTIMEM_UNHIDE_ID_ULTIMEM   0x11    /* 8MB Flash, 1MB RAM */
#define ULTIMEM_UNHIDE_ID_VICMIDI   0x12    /* 512K Flash, 512K RAM */

extern char     ultimem_is_installed (void);
extern unsigned ultimem_get_size (void);

extern void __fastcall__ ultimem_send_command (char);
extern void __fastcall__ ultimem_burn_byte (unsigned short addr, char);
extern void              ultimem_erase_chip (void);
extern void __fastcall__ ultimem_erase_block (char);

extern void __fastcall__ ultimem_copy_rom2ram (long src, long dst, unsigned size);

// Map in memory bank a pointer is pointing to in another block config.
extern void * __fastcall__ ultimem_map_ptr (void * destbase, unsigned short * destreg, void * ptr, unsigned short * blockregs);

#endif /* #define ULTIMEM_H */
