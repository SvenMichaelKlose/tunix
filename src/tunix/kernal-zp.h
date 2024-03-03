#ifndef _TUNIX_KERNAL_ZP_H
#define _TUNIX_KERNAL_ZP_H

#define FNLEN   (*(char *) 0xb7)    // File name length
#define LFN     (*(char *) 0xb8)    // Logical file number
#define SA      (*(char *) 0xb9)    // Secondary address
#define FA      (*(char *) 0xba)    // Device number
#define FNAME   (*(char **) 0xbb)   // File name pointer
#define ST      (*(char *) 0x90)    // Serial line status byte
#define LDTND   (*(char *) 0x98)    // Number of open files
#define DFLTN   (*(char *) 0x99)    // Logical input file number
#define DFLTO   (*(char *) 0x9a)    // Logical output file number

#define SAP     (*(char **) 0xac)   // Load/save current address
#define SAL     (*(char *) 0xac)
#define SAH     (*(char *) 0xad)
#define EAP     (*(char **) 0xae)   // Load/save end address
#define EAL     (*(char *) 0xae)
#define EAH     (*(char *) 0xaf)

#define LAT     (*(char *) 0x0259)  // File number table
#define FAT     (*(char *) 0x0263)  // Device number table
#define SAT     (*(char *) 0x026d)  // Secondary address table

#endif // #ifndef _TUNIX_KERNAL_ZP_H
