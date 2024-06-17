Building
========

Before doing anything else after cloning this repository,
please do this first to fetch all third-party code:

~~~sh
git submodule update --init --recursive
~~~

The build configuration is file in 'src/config'.  You can
use 'src/config.example' as a template.  TARGETs are:
'c128', 'c16', 'c64', 'plus4', 'vic20' and 'unix'.  Some
may not work.  It may look like this:

~~~
TARGET = vic20
NDEBUG = 1 # No extra checks.
~~~

After you have created your config build tools have to
be made and the world for the desired target can be built.
You can do than in one run:

~~~sh
make all
~~~

Running the tests is also a good idea:

~~~sh
make test
~~~

If you want to create disk images and archives for all
targets just do:

~~~sh
make allworlds
~~~
