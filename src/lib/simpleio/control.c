#ifdef __CC65__
    #ifndef __CBM__
        #define __CBM__
    #endif
#include <cbm.h>
#include <ingle/cc65-charmap.h>
#endif // #ifdef __CC65__

#include <ctype.h>
#include <string.h>
#include <stdbool.h>

#include <simpleio/libsimpleio.h>
#include <simpleio/control.h>

con_cmd con_commands[] = {
    { 0, cmd_null },    // 0
    { 2, cmd_goto },    // 1
    { 1, cmd_clr },     // 2
    { 1, cmd_set },     // 3
    { 0, cmd_get },     // 4
    { 0, cmd_getx },    // 5
    { 0, cmd_gety },    // 6
    { 0, cmd_null },    // 7
    { 0, cmd_null },    // 8
    { 0, cmd_null },    // 9
    { 0, cmd_lf },      // 10
    { 0, cmd_null },    // 11
    { 0, cmd_clrscr },  // 12
    { 0, cmd_cr },      // 13
};

cmd_handler cmd_current;
char cmd_params_needed;
char cmd_params[2];
bool term_direct_mode;

bool FASTCALL
simpleio_control (char c)
{
    if (fnout == STDOUT || fnout == STDERR) {
        // Get control code argument, call handler.
        if (cmd_params_needed) {
            cmd_params[(int) --cmd_params_needed] = c;
            if (!cmd_params_needed) {
                cmd_current ();
                cmd_current = NULL;
            }
            return true;
        }

        // Init control sequence, or call handler.
        if (c <= 13) {
            if ((cmd_params_needed = con_commands[(int) c].num_params))
                cmd_current = con_commands[(int) c].handler;
            else
                con_commands[(int) c].handler ();
            return true;
        }
    }

    return false;
}
