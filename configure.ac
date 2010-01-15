# Process this file with autoconf to produce a configure script.
# $Id$

AC_INIT(rmodel,03.10,[ncl@astrax.fis.ucm.es])
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([config.h.in])
AM_INIT_AUTOMAKE([foreign])

AC_REVISION($Revision$)
AC_PROG_FC

STTL_LIB_PGPLOT
LIBS="$LIBS $PGPLOT_LIBS"

DEFAULT_DATA_DIR="/usr/local/share/rmodel"
DATA_DIR=${DATA_DIR="$DEFAULT_DATA_DIR"}
AC_ARG_VAR(DATA_DIR,[Directory to install definition files and models])

AC_CONFIG_FILES([Makefile 
                 src/Makefile
                 src/dimensions.inc
                 src/rmodel.inc
                 src/version.inc
                 models/Makefile
                 m4/Makefile
                 definitions/Makefile])
AC_OUTPUT

