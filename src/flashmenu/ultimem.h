#ifndef ULTIMEM_H
#define ULTIMEM_H

#define ULTIMEM_CONFIG0     ((char *) 0x9ff0)
#define ULTIMEM_CONFIG1     ((char *) 0x9ff1)
#define ULTIMEM_CONFIG2     ((char *) 0x9ff2)
#define ULTIMEM_ID          ((unsigned short *) 0x9ff3)
#define ULTIMEM_BANK_RAM    ((unsigned short *) 0x9ff4)
#define ULTIMEM_BANK_IO     ((unsigned short *) 0x9ff6)
#define ULTIMEM_BANK_1      ((unsigned short *) 0x9ff8)
#define ULTIMEM_BANK_2      ((unsigned short *) 0x9ffa)
#define ULTIMEM_BANK_3      ((unsigned short *) 0x9ffc)
#define ULTIMEM_BANK_5      ((unsigned short *) 0x9ffe)

void __fastcall__ ultimem_send_command (char);
void __fastcall__ ultimem_write_byte (unsigned short addr, char);
void ultimem_erase_chip (void);
void __fastcall__ ultimem_erase_block (char);

#endif /* #define ULTIMEM_H */
