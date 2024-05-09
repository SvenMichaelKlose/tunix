Bielefeld DB CLI
================

A tool for managing CSV databases.

* first column is the ID
* second column is the key

~~~sh
# Show database info.
db <db>

# Create a new database.
# File <db> must not exist.
db <db> create

# Return one or more records by ID.
db <db> get [<id>...]

# Update whole record.
db <db> set <row>

# Add record.  Returns its ID.
db <db> add <row>...

# Search column by pattern.
db <db> find <col> <pattern>...

# Remove record(s) by ID.
db <db> rm [<id>...]

# Update record.
# (Key cannot be updated yet.)
db <db> update <id> [<col>=<value>]...
~~~

~~~sh
db users find name pixel | db rm
~~~
