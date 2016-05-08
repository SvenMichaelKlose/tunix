.export column_addrs_lo, column_addrs_hi
.data

column_addrs_lo: ;@(maptimes [low (+ charset (* 16 screen_rows _))] screen_columns)
column_addrs_hi: ;@(maptimes [high (+ charset (* 16 screen_rows _))] screen_columns)
