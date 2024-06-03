Building
========

Before doing anything else after cloning this repository,
please do this first to fetch all third-party code:

~~~sh
git submodule update --init --recursive
~~~

The build configuration is file in 'src/config'.  You can
use 'src/config.example' as a template.  Working TARGETs
are: 'c64', 'c128', 'plus4', 'vic20' and 'unix'.

After you have created your config you can start building
for your specified target right away:

~~~sh
make all
~~~

Running the tests is also a good idea:

~~~sh
make all test
~~~

If you want to create disk images and archives for all
targets just do:

~~~sh
make allworlds
~~~

Enjoy!
