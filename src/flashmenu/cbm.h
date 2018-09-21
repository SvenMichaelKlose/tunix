#ifndef CBM_H
#define CBM_H

char cbm_readst ();
int __fastcall__ cbm_opendir (char * pathname, char device);
int __fastcall__ cbm_readdir (char * buffer);
int cbm_closedir ();
char cbm_read_char (void);

#endif /* #ifndef CBM_H */
