Building
========

Before doing anything else after cloning this repository,
please do this first to fetch all third-party code:

~~~sh
git submodule update --init --recursive
~~~

The build configuration is file in 'src/config'.  You can
copy over 'src/config.example' as a template, which already
has the release defaults.

After you have created your configuration, build everything
like this:

~~~sh
make allworlds
~~~

You then find all targets compiled in directory 'tunix'.
