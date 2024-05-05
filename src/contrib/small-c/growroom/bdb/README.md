Bielefeld DB
============

This is a key/value database library and
every Unixoid operating system should
have one.  In traditional Unices it is
the "Berkeley DB" but this one is a lot
simpler and intended to be used on tiny
computers with no fancy memory
management.

Features:

* Light-weight; written in ANSI-C.
* Embedded; for use in applications.
* Simple API.
* Records of variable lengths and any
  format.  Must contain their keys.
* Fast; using b-trees and LRU lists.
* Secondary storage to free main memory.
* Long-term storage

Operations:

* Add record
* Find record
* Map record
* Get next/previous record ordered
* Empty cache to storage

Limitations:

* IDs of mapped records are not
  validated (on storage level).
* Records cannot be removed from
  storage.  It continues growing.
* Maximum number of records and storage
  size is defined at compile-time.
* One index per database only.

Future enhancements:

* ID validation for testing at least by
  adding ID databases.
