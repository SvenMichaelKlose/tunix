#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([g], [0.1], [pixel@hugbox.org])
AM_INIT_AUTOMAKE([foreign])
LT_INIT
AC_CONFIG_MACRO_DIR([m4])

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.
AC_C_INLINE
AC_TYPE_SIZE_T

# Checks for library functions.
AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_CHECK_FUNCS([bzero gettimeofday munmap strerror])

AC_CONFIG_FILES([Makefile
                 mkfs/Makefile
				 ])
AC_OUTPUT
