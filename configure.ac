# Process this file with autoconf to produce a configure script.
# $Id$

AC_INIT(rmodel,3.2.0,[cardiel@fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([foreign])

AC_REVISION($Revision$)
AC_PROG_F77
#AC_PROG_FC

# Safety check
AC_CONFIG_SRCDIR([src/rmodel.f])

AC_MSG_CHECKING([uname -s for detecting host operating system])
OSNAME=`uname -s`
case "$OSNAME" in
  Darwin*)
    AC_MSG_RESULT(Darwin)
    # Checks for libraries
    LDFLAGS="$LDFLAGS -L/opt/local/lib"
  ;;
  Linux*)
    AC_MSG_RESULT(Linux)
    # Checks for libraries
    LDFLAGS="$LDFLAGS -L/usr/local/pgplot"
  ;;
esac
AC_MSG_RESULT(LDFLAGS set to... $LDFLAGS)
AC_CHECK_LIB([X11], main,,AC_MSG_ERROR(X11 library not found))
AC_CHECK_LIB([pgplot], main,,AC_MSG_ERROR(pgplot library not found))
LIBS="$LIBS"

DEFAULT_DATA_DIR="/usr/local/share/rmodel"
DATA_DIR=${DATA_DIR="$DEFAULT_DATA_DIR"}
AC_ARG_VAR(DATA_DIR,[Directory to install definition files and models])

AC_CONFIG_FILES([Makefile 
                 src/Makefile
                 src/dimensions.inc
                 src/rmodel.inc
                 src/version.inc
                 models/Makefile
                 definitions/Makefile])
AC_OUTPUT

