File database
=============

This database holds records in memory
and moves least recently used ones to
secondary storage if that runs low.
Records have variable sizes and unique
keys and IDs for fast look-ups.  They
cannot be removed.

Pointers to records can be requested
with bdb_map() by passing it the record
ID.  The record then is marked as being
the most-recently used one, so it'll
move to secondary storage if it is (most
probably) not used again.  The minimum
number of records in memory should
accordingly be as large as the number of
records the application must be able to
handle simultaneaously, or the database
is unusable.  If the life time of an
operation on a record cannot be
predicted, the record can be locked to
keep it from vanishing from memory.

Records can be added with bdb_add().
But only modifications bdb_map()'ed
records will last.  bdb_find() does a
search for a particular key, based on
the provided test function which must
behave like strcmp().

Secondary storage is assumed to be a
block of memory that grows automatically
on writes beyond its size.

# B-tree indexed cache

The cache has btrees over keys and IDs.
IDs are the offsets on secondary
storage.  The bits of an ID are reversed
in order to generate a key that forces
the btree to be balanced.
The downside of the b-tree cache is its
additional demand for memory for each
node.

# Brute search cache

The API often does a b-tree search for
a key, followed by a look-up by ID,
which is also a b-tree search.  A brute
force searcu through the first,
most-recently used records makes up for
that, allowing the database code to be
much cleaner.
