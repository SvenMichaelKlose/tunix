Bielefeld DB
============

# NAME

bdb - lightweight key/value database library.

# SYNOPSIS

```c
#include <bdb.h>

int    bdb_add   (bdb *, void *key, void *data, size_t);
int    bdb_find  (bdb *, void *key);
void * bdb_map   (bdb *, dbid_t);
int    bdb_next  (bdb *, dbid_t);
int    bdb_prev  (bdb *, dbid_t);
void   bdb_flush (bdb *, void);
```

# DESCRIPTION

Bielefeld DB (bdb) is a lightweight embedded key/value
database library written in ANSI-C.  It offers a simple API
to handle records of varying lengths and formats.
Keys are efficiently indexed using B-trees.  The database
can offload records to secondary storage via a user-defined
interface, freeing up main memory.

# FEATURES

* Light-weight: Small footprint, written in ANSI-C.
* Embedded: Suitable for embedding in applications.
* Simple API: Easy to use, providing core database
  functionality.
* Flexible Records: Supports records of variable
  lengths and any format.
* Fast: Utilizes B-trees and LRU lists for quick access.
* Secondary Storage: Allows long-term data storage and
  freeing up main memory.

# LIMITATIONS

* No Record Validation: IDs of mapped records are not
  validated on the storage level.
* No Record Deletion: Records cannot be removed from
  storage; the storage file will continue to grow.
* Predefined Capacity: The maximum number of records
  and storage size are defined at compile time.
* Single Index Per Database: Only one index per
  database is supported.

# FUTURE ENHANCEMENTS

* List-only Cache: Implement a cache without B-trees.
* Key-only Cache: Cache only keys and IDs.
* ID Validation: Add ID databases for record ID
  validation.
* No Keys: Option to use records without keys.

# EXAMPLES

See tests.

# AUTHORS

Maintained by Sven Michael Klose <pixel@hugbox.org>.

# SEE ALSO

* bdb.h - Header file defining the Bielefeld DB API functions
