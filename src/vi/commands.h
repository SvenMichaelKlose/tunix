#ifndef COMMANDS_H
#define COMMANDS_H

extern void cmd_open_above    (void);
extern void cmd_open_below    (void);
extern void cmd_enter         (void);
extern void cmd_change_till_line_end  (void);
extern void cmd_delete_till_line_end  (void);
extern void cmd_delete_char   (void);
extern void cmd_replace_char  (void);
extern void cmd_join          (void);
extern void cmd_write_file    (void);
extern void cmd_read_file     (void);

#endif // #ifndef COMMANDS_H
