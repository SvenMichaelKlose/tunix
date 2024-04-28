File database
=============

This database holds records in memory
and moves least recently used ones to
secondary storage if that runs low.
Records have variable sizes and unique
keys and IDs for fast look-ups.  They
cannot be removed.

Pointers to records can be
requested with db_map() by passing it
the record ID.  The record then is
marked as being the most-recently used
one, so it'll move to secondary storage
if it is (most probably) not used again.
The minimum number of records in memory
should accordingly be as large as the
number of records the application must
be able to handle simultaneaously, or
the database is unusable.

Records can be added with db_add().  But
only modifications db_map()'ed records
will last.  db_find() does a search for
a particular key, based on the provided
test function which must behave like
strcmp().

Secondary storage is assumed to be a
block of memory that grows automatically
on writes beyond its size.

# B-tree indexed cache

Records in memory and secondary storage
are separately indexed using b-trees.
