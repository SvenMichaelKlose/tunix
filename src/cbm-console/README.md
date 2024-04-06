C1541 DOS compatibility tests
=============================

To help implement new filesystems.

Takes the device number as it's argument.
This will test device #12:

~~~
LOAD"FSTEST",8
RUN:REM12
~~~
