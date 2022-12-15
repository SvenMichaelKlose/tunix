#ifndef COMMANDS_H
#define COMMANDS_H

extern void cmd_open_above      (void);
extern void cmd_open_below      (void);
extern void cmd_enter           (void);
extern void cmd_change_till_line_end  (void);
extern void cmd_delete_till_line_end  (void);
extern void cmd_delete_line     (void);
extern void cmd_delete_char     (void);
extern void cmd_replace_char    (void);
extern void cmd_join            (void);
extern void cmd_set_passphrase  (void);
extern void cmd_write_file      (void);
extern void cmd_read_file       (void);

// unfinished
extern void cmd_delete          (void);
extern void cmd_change          (void);
extern void cmd_yank            (void);
extern void cmd_paste           (void);
extern void cmd_follow          (void);
extern void cmd_paste_above     (void);
extern void cmd_paste_below     (void);
extern void cmd_toggle_visual_mode (void);
extern void cmd_quit           (void);

#endif // #ifndef COMMANDS_H
