# UltiFS

This is merely a proposal.

## Architecture

The UltiFS is made of three layers.

### Bank layer

This is the lowest layer which deals with modifications of banks
that have already been flashed.

### File store

This middle layer deals with files only which are identified by
an integer.
File 0 is reserved for the application the boot loader is starting.
File 1 is the root directory.

### File system

This top layer introduces directory files.

## Bank layout

The first eight banks are used for booting an bookkeeping.

### Bank 0

The bank that is booted from.

* Autostart signature
* Boot loader
* Easyflash API
* UltiFS file store API

The boot loader uses the file store to load file #0.

### Bank 1/2

Two banks used in turn for just–in–time garbage collection.

####  Bank allocations map

A bitmap of allocated banks. 128 bytes for 8Mb.

####  Replacement banks

If a bank in the file store is to be modified a new bank
is allocated and entered into this list.

#### File index

List of file positions (3 bytes each) and optinal address of modfied records (2 bytes each).

### Bank 3/4

#### Blocks

A binary tree of blocks, allocated and unallocated. Can continue in any
other bank.  Requires no stack for traversal.

1   status
3   start
2   size
3   next
3   update
3   parent
3   child left
3   child right

### Banks 119–127

64K segment for garbage collection.

## GC

* Clean up bank replacements.
* Clean up node list.
* Make new node list with modified addresses.
* Make replacement for a bank.
* Make corrected file index.
* Commit replacement bank to original.
* Correct bank allocation map
