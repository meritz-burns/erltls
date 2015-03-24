AC_PREREQ([2.69])
AC_INIT([libtls-erlang], [0.1], [rebecca@meritz.com])
AM_INIT_AUTOMAKE
AC_CONFIG_HEADERS([config.h])
AC_CHECK_HEADERS([stdlib.h string.h unistd.h])
AC_FUNC_MALLOC
AX_PTHREAD([
            LIBS="$PTHREAD_LIBS $LIBS"
            CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
            LDFLAGS="$LDFLAGS $PTHREAD_CFLAGS"
            CC="$PTHREAD_CC"
            ],
            [AC_MSG_ERROR([pthreads was not found])])
PKG_CHECK_MODULES([TLS], [libtls])
AC_ERLANG_CHECK_LIB([erl_interface],
                    [],
                    [AC_MSG_ERROR([erl_interface was not found])])
AC_ERLANG_SUBST_ROOT_DIR
AC_PROG_CC
AC_PROG_CC_STDC
AC_CONFIG_FILES([
                 Makefile
                 c_src/Makefile
                 ])
AC_OUTPUT
