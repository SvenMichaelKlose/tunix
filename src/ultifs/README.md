# Ultimem file system

A file system for the Ultimem Flash ROM made of several layers:

* The block store is the lowest layer which keeps track of 8Kb chunks.
* The file store which stores files identified by numbers in blocks and takes care of byte–resolution lengths and fragmentation.
* The API layer which discriminates between files and directories (which are also files to the file store).
