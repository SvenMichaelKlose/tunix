#ifndef KERNAL_H
#define KERNAL_H

#define MAX_FNLEN   32      // Maximum file name length (not including zero termination).
#define MAX_FPARAMS 8       // Maximum # of parameters per command.
#define NUM_LFN     255     // No limit. LFNs 128>= should add extra line feeds. (pixel)

typedef unsigned char uchar;

extern uchar accu;
extern uchar xreg;
extern uchar yreg;
extern uchar flags;
extern uchar cfg;
extern uchar proc_blk1;
extern uchar proc_blk2;
extern uchar proc_blk3;
extern uchar proc_blk5;

// CPU flags
#define FLAG_C       1
#define FLAG_Z       2
#define FLAG_I       4
#define FLAG_D       8
#define FLAG_B       16
#define FLAG_UNUSED  32
#define FLAG_V       64
#define FLAG_N       12

/* KERNAL zero page data */
#define FNLEN   (*(uchar *) 0xb7)    // File name length
#define LFN     (*(uchar *) 0xb8)    // Logical file number
#define SA      (*(uchar *) 0xb9)    // Secondary address
#define FA      (*(uchar *) 0xba)    // Device number
#define FNAME   (*(char **) 0xbb)    // File name pointer
#define STATUS  (*(uchar *) 0x90)    // Serial line status byte
#define LDTND   (*(uchar *) 0x98)    // Number of open files
#define DFLTN   (*(uchar *) 0x99)    // Logical input file number
#define DFLTO   (*(uchar *) 0x9a)    // Logical output file number

#define SAP     (*(char **) 0xac)    // Load/save current address
#define SAL     (*(uchar *) 0xac)
#define SAH     (*(uchar *) 0xad)
#define EAP     (*(char **) 0xae)    // Load/save end address
#define EAL     (*(uchar *) 0xae)
#define EAH     (*(uchar *) 0xaf)

/*
#define LAT     (*(uchar *) 0x0259)  // File number table
#define FAT     (*(uchar *) 0x0263)  // Device number table
#define SAT     (*(uchar *) 0x026d)  // Secondary address table
*/

/* STATUS: Serial line error codes */
#define STATUS_NO_DEVICE       0x80
#define STATUS_END_OF_FILE     0x40
#define STATUS_CHECKSUM_ERROR  0x20
#define STATUS_READ_ERROR      0x10
#define STATUS_LONG            0x08
#define STATUS_SHORT           0x04
#define STATUS_TIMEOUT_READ    0x02
#define STATUS_TIMEOUT_WRITE   0x01

/* KERNAL error codes */
#define OSERR_TOO_MANY_FILES         1
#define OSERR_FILE_ALREADY_OPEN      2
#define OSERR_FILE_NOT_OPEN          3
#define OSERR_FILE_NOT_FOUND         4
#define OSERR_DEVICE_NOT_PRESENT     5
#define OSERR_FILE_NOT_IN            6
#define OSERR_FILE_NOT_OUT           7
#define OSERR_MISSING_FILE_NAME      8
#define OSERR_ILLEGAL_DEVICE_NUMBER  9

/* Device error codes (read from command channel #15) */
#define ERR_OK                   0
#define ERR_FILES_SCRATCHED      1  // Not an error when < 20.
#define ERR_BLOCK_HEADER_NOT_FOUND 20 // Block header not found.
#define ERR_NO_SYNC_CHAR           21 // No sync character.
#define ERR_DATA_BLOCK_NOT_PRESENT 22 // Data block not present.
#define ERR_BLOCK_CHECKSUM       23 // Checksum error in data block.
#define ERR_BYTE_DECODING        24 // Byte decoding error.
#define ERR_WRITE_ERROR          25 // Write-verify error.
#define ERR_WRITE_PROTECT_ON     26
#define ERR_HEADER_CHECKSUM      27 // Checksum error in header.
#define ERR_LONG_DATA            28 // Long data block.
#define ERR_DISK_ID_MISMATCH     29
#define ERR_SYNTAX               30 // General syntax.
#define ERR_INVALID_COMMAND      31 // Invalid command.
#define ERR_LONG_COMMAND         32 // Command is longer than 58 chars.
#define ERR_INVALID_FILE_NAME    33 // Invalid file name.
#define ERR_NO_FILE_GIVEN        34
#define ERR_INVALID_CTRL_COMMAND 39 // Invalid command on control channel #15.
#define ERR_RECORD_NOT_PRESENT   50
#define ERR_OVERFLOW_IN_RECORD   51
#define ERR_FILE_TOO_LARGE       52
#define ERR_FILE_OPEN_FOR_WRITE  60
#define ERR_FILE_NOT_OPEN        61
#define ERR_FILE_NOT_FOUND       62
#define ERR_FILE_EXISTS          63
#define ERR_FILE_TYPE_MISMATCH   64
#define ERR_NO_BLOCK             65
#define ERR_ILLEGAL_POSITION        66 // Illegal track and sector.
#define ERR_ILLEGAL_SYSTEM_POSITION 67 // Illegal system track and sector.
#define ERR_NO_CHANNEL           70
#define ERR_DIRECTORY_ERROR      71
#define ERR_DISK_FULL            72
#define ERR_DOS_MISMATCH         73
#define ERR_DRIVE_NOT_READY      74

extern uchar global_lfns[256];

typedef struct _channel {
    char *   name;          // File name
    bfile *  file;          // UltiFS file
    char     sa;            // Secondary address
    char     is_buffered;   // As of now: reads from buffer with
                            // generated content (e.g. directory listing).

    char *   buf;
    char *   bufwptr;       // Fill pointer
    char *   bufrptr;       // Read pointer
} channel;

#ifdef TEST
extern bool has_prefix;
extern bool has_params;
extern uchar num_params;

extern char fullname[MAX_FNLEN];
extern char prefix[MAX_FNLEN];
extern char pathname[MAX_FNLEN];
extern char params[MAX_FNLEN];
extern char * param_list[MAX_FPARAMS];
extern char filename[MAX_FNLEN];

extern void parse_pathname (void);
#endif

#endif // #ifndef KERNAL_H
