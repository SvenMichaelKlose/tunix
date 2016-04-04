screen_columns  = 20                                                            
screen_rows     = 12
screen_width    = @(* 8 screen_columns)
screen_height   = @(* 16 screen_rows)

screen  = $1000
colors  = $9400
charset = $1100
charset_size = @(* screen_rows screen_columns 16)
char_offset = 16
