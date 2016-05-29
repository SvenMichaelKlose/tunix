#ifndef ULTIMEM_H
#define ULTIMEM_H

#define ULTIMEM_CONFIG0     ((char *) 0x9ff0)
#define ULTIMEM_CONFIG1     ((char *) 0x9ff1)
#define ULTIMEM_CONFIG2     ((char *) 0x9ff2)
#define ULTIMEM_ID          ((unsigned short *) 0x9ff3)
#define ULTIMEM_RAM         ((unsigned short *) 0x9ff4)
#define ULTIMEM_IO          ((unsigned short *) 0x9ff6)
#define ULTIMEM_BLK1        ((unsigned short *) 0x9ff8)
#define ULTIMEM_BLK2        ((unsigned short *) 0x9ffa)
#define ULTIMEM_BLK3        ((unsigned short *) 0x9ffc)
#define ULTIMEM_BLK5        ((unsigned short *) 0x9ffe)

void __fastcall__ ultimem_send_command (char);
void __fastcall__ ultimem_write_byte (unsigned short addr, char);
void ultimem_erase_chip (void);
void __fastcall__ ultimem_erase_block (char);

char ultimem_is_installed (void);
unsigned short ultimem_get_size (void);

#endif /* #define ULTIMEM_H */
