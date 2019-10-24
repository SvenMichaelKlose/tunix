( fixed-point trig - John S. James )
( Forth Dimensions V4 N1 )

( trig lookup routines - with sine *10000 table)

( sine, 0-90 degrees only)
create sintable
0000 ,
0175 , 0349 , 0523 , 0698 , 0872 , 1045 , 1219 , 1392 , 1564 , 1736 ,
1908 , 2079 , 2250 , 2419 , 2588 , 2756 , 2924 , 3090 , 3256 , 3420 ,
3584 , 3746 , 3907 , 4067 , 4226 , 4384 , 4540 , 4695 , 4848 , 5000 ,
5150 , 5299 , 5446 , 5592 , 5736 , 5878 , 6018 , 6157 , 6293 , 6428 ,
6561 , 6691 , 6820 , 6947 , 7071 , 7193 , 7314 , 7431 , 7547 , 7660 ,
7771 , 7880 , 7986 , 8090 , 8192 , 8290 , 8387 , 8480 , 8572 , 8660 ,
8746 , 8829 , 8910 , 8988 , 9063 , 9135 , 9205 , 9272 , 9336 , 9397 ,
9455 , 9511 , 9563 , 9613 , 9659 , 9703 , 9744 , 9781 , 9816 , 9848 ,
9877 , 9903 , 9925 , 9945 , 9962 , 9976 , 9986 , 9994 , 9998 , 10000 ,

( sine and cosine table-loopup routines)
: s180 ( n-> n . returns sine, 0-180 degrees)
  dup 90 > ( if greater than 90 degrees,)
  if 180 swap - then ( subtract from 180)
  2* sintable + @ ( then take sine)
  ;

: sin ( n -> sine, return sine of any number of degrees)
    360 mod ( bring within + or - 360)
    dup 0< if 360 + then ( if negative, add 360)
    dup 180 > ( test if greater than 180)
  if 180 - s180 negate ( if so, subtract 180, negate sine)
  else s180 then ( otherwise, straightforward)
;

: cos ( n cosine.)
    360 mod ( prevent overflow near 32,767)
    90 + sin ; ( cosine is sine with 90 degrees phase shift)
