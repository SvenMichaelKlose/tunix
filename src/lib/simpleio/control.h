#ifndef __SIMPLEIO_CONTROL_H__
#define __SIMPLEIO_CONTROL_H__

#define TERM_CMD_CLRSCR  12 // Clear screen.
#define TERM_CMD_GOTO    1  // Position cursor.
#define TERM_CMD_CLR     2  // Clear flags.
#define TERM_CMD_SET     3  // Set flags.

#define TERM_FLAG_CURSOR   1    // Cursor visibility.
#define TERM_FLAG_REVERSE  2    // Reverse mode.

// Platform-specific handlers.
extern void cmd_null (void);
extern void cmd_goto (void);
extern void cmd_clr (void);
extern void cmd_set (void);
extern void cmd_lf (void);
extern void cmd_cr (void);
extern void cmd_clrscr (void);

typedef void (*cmd_handler) (void);

typedef struct _con_cmd {
    char        num_params;
    cmd_handler handler;
} con_cmd;

extern con_cmd con_commands[];

extern cmd_handler cmd_current;
extern char cmd_params_needed;
extern char cmd_params[2];

extern bool FASTCALL simpleio_control (char c);

#endif // #ifndef __SIMPLEIO_CONTROL_H__