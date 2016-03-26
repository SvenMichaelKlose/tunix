# Ultimem file system

This is a file system for the Ultimem Flash ROM. It is constructed of
several layers to make future improvements easier to handle.

The file system can handle a maximum of 8192 files. Its maximum overall
capacity depends on the logical block size.

# Block store

The block store is the lowest layer which keeps track of 8Kb chunks and keeps
track of where the file system is stored on the medium where you can house
multiple file systems.

The block store must be stored as a chunk of continuous blocks. The first
bank contains optional ROM autostart info, file system information and
optional boot code. It is followed by a block allocation map which may
reside on the same bank. Finally the data blocks follow – each is a bank
of 8Kb.

A maximum of 512Mb can be handled since the block allocation map is limited
to be kept on a single bank.

# File store

This layer is responsible for spreading files with unique integer IDs across
blocks. Block chains are stored in a single file allocation map, the same
like FAT file system do it.

# API layer

This is where directories (which are also files) are introduced and files
get their names.
