Installation
============

Starting with version 3.0, the installation procedure is based on the popular
autotools configure and make. The present version has been developed under Mac
OS X, Snow Leopard using g95 as Fortran compiler, but it has also been tested
on Linux (Fedora, Ubuntu, CentOS, openSUSE and Debian) using gfortran. Before
you install rmodel, make sure that PGPLOT is already installed in your
computer. Some instructions about how I typically install PGPLOT under Mac OS X
and Linux are given `here
<http://pendientedemigracion.ucm.es/info/Astrof/software/howto/howto-pgplot.html>`_.

The installation procedure of rmodel will generate a compiled binary file
called rmodel which, by default, will be installed as
``/usr/local/bin/rmodel``. In addition, some auxiliary files (e.g. the model
predictions) also need to be accesible during execution time. These files are
installed by default under the directory ``/usr/local/share/rmodel``. Note that
the installation under those by default directories requires root privileges.
If this is not your case, those directories can be easily modified as explained
below.

To install **rmodel** you need to perform the following steps:

1.- Download the latest version of the code from github:

::

   $ git clone https://github.com/nicocardiel/rmodel

2.- Install the program by executing:

::

   $ cd rmodel
   $ ./configure

.. note:: Mac users can easily indicate the Fortran compiler using
      ``./configure F77=gfortran-mp-5``.

If you do not have root privileges, you can install **rmodel** in any local
directory where you have write access. In this case you need a personalized
directory installation, that can easily be specified using ``--bindir`` to indicate
the location of the executable file, and ``DATA_DIR`` to specify the directory
where the auxiliary files must be installed. For example the instruction:

::

   $ ./configure --bindir=/home/userxxx/bin DATA_DIR=/home/userxxx/share/rmodel

will prepare the installation to place the binary file rmodel as 
``/home/userxxx/bin/rmodel`` and the auxiliary files under 
``/home/userxxx/share/rmodel``.

If configure is not able to locate ``PGPLOT``, it is possible to skip this
problem by specifying the directory where this graphics package is installed
using the parameter ``LDFLAGS``. For example:

::

   $ ./configure --bindir=/home/userxxx/bin DATA_DIR=/home/userxxx/share/rmodel LDFLAGS=-L/usr/local/pgplot

3.- After successfully executing configure, the system is ready to proceed with
the actual compilation of the code:

::

   $ make

If you get an error installing the software, check that the fortran compiler
you are using to install ``PGPLOT`` and to compile **rmodel** is the same. If this is
not the case, force the compiler to be the same by indicating it when executing
configure with the help of the paramter ``F77``. For example, if ``PGPLOT`` was
installed using the g95 compiler, execute:

::

   $ make clean
   $ ./configure F77=g95
   $ make

4.- Finally, you must finish the installation procedure by placing the
executable and auxiliary files in their corresponding directories. If you are
installing the software in the by default directories (``/usr/local/...``), you
need root privileges:

::

   $ sudo make install

or

::

   $ su
   # make install

Otherwise, if you are installing the software in custom defined directories for
which you do not need system privileges, you can simple execute:

::

   $ make install

5.- You can optionally clean the intermediate object files generated during the
compilation procedure:

::

   $ make clean
