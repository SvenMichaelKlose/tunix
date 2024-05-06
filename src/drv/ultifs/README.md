UltiFS ROM file system
======================

This is a Flash ROM file system for UltiMem expansion under consutruction.

# Structure

The file system starts at ROM address $10000.

UltiFS holds a tree list of blocks, each block containing either a file or a
directory.  Both types have the same layout: a name, a pointer to the next
never version and a pointer to the next block in the directory, followed by
file data or, if it's a directory, the offset of the first file of the
directory.

UltiFS is copy-on-write but for whole files only.  The ROM is filled with
blocks from start to end until it runs out of space.

# DOS commands

At this moment long command names are not supported.

## I (INITIALIZE)

Always responds with "OK".

## POSITION (P) – Seek in file.

Can be used with any file type.

~~~
P{channel}{24 bit position}
~~~
