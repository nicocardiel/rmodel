#!/bin/csh
#------------------------------------------------------------------------------
# Version 21-October-2008
#------------------------------------------------------------------------------
# To install rmodel properly, you must follow these steps:
# make include
# make
# make clean
#------------------------------------------------------------------------------
VERSION = 02.6
#------------------------------------------------------------------------------
# verify that the required libraries are properly set
PGPDIR  = /usr/local/pgplot
X11DIR  = /usr/lib64
#------------------------------------------------------------------------------
# Set the appropiate compilers
FCOMPIL = gfortran -g -O3 -Wall
#------------------------------------------------------------------------------
#               Nothing SHOULD be modified below this comment line
#------------------------------------------------------------------------------
# macro definitions
FSOURCE = cextrae.f \
          chequea_area.f \
          downhill.f \
          fextrae.f \
          findindex.f \
          fmean0.f \
          fstatistic.f \
          iextrae.f \
	  include_external_data.f \
          indexinfo.f \
          indexr.f \
          inside_rombo.f \
          iofunctions.f \
          plot_diagram.f \
          ranred.f \
          return_arrays.f \
          rmodel.f \
          rspace.f \
          set_my_colors.f \
          systemfunction.f \
          tolog77_string.f \
          transform.f
FOBJECT = $(FSOURCE:.f=.o)
# Default rule to create program
rmodel:  $(FOBJECT)
	$(FCOMPIL) -o $@ $(FOBJECT) -L$(PGPDIR) -L$(X11DIR) -lpgplot -lX11
# Target to clean object modules
clean:    $(FOBJECT)
	\rm $(FOBJECT) rmodel.inc
# Target to touch source modules
touch:
	touch $(FSOURCE)
# Target to create the file rmodel.inc
include:
	echo "         CHARACTER*255 RMODEL_DIR_" > rmodel.inc
	echo "         PARAMETER (RMODEL_DIR_=" >> rmodel.inc
	echo "     +    '"`pwd`"')" >> rmodel.inc
	touch $(FSOURCE)
	rm -f version.inc
	echo "        CHARACTER*4 VERSION" > version.inc
	echo "        PARAMETER(VERSION='$(VERSION)')" >> version.inc
	touch $(FSOURCE)
# second level dependencies
.f.o: $(FSOURCE)
	$(FCOMPIL) -c $?
# definitions
.PRECIOUS: rmodel
