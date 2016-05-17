#ifndef ULTIMEM_H
#define ULTIMEM_H

void __fastcall__ ultimem_send_command (char);
void __fastcall__ ultimem_write_byte (unsigned short addr, char);
void ultimem_erase_chip (void);
void __fastcall__ ultimem_erase_block (char);

#ifndef /* #define ULTIMEM_H */
