#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.57)
AC_INIT(semper, 7.0beta, L-marks@northwestern.edu)
AM_INIT_AUTOMAKE
#AC_PROG_LIBTOOL
AC_CONFIG_HEADER([config.h])
AC_PREFIX_DEFAULT([/usr/local/semper-7.0b])
# Patches -- set to zero for the moment
PATCHES=""

#Some general flags
AC_ARG_WITH(gcc, [  --with-gcc              use gcc, g++ and gfortran/g77, default], ok=$withval, ok=yes)
if test "$ok" = "yes"; then
	CC=gcc
#	F77=g77
        AC_PROG_FC(gfortran g77)
        F77=$FC
	CXX=g++
	echo "Using gcc compilers, default"
fi

# Append legacy Fortran flags to FFLAGS
if test -z "$FFLAGS"; then
  FFLAGS="-std=legacy -fno-automatic"
else
  FFLAGS="$FFLAGS -std=legacy -fno-automatic"
fi
export FFLAGS

# Some linux/PC flags
AC_ARG_WITH( CPU,
  [  --with-CPU[=Val]          Defines the cpu in linux],
  [wincpu=$withval ; export wincpu ], 
  wincpu="None"
   ) 
export wincpu
# Some linux/PC flags
AC_ARG_ENABLE(portable, 
	[  --enable-portable	  portable compilation for gcc -march/-mtune]],
	portable=$enableval, portable="no"
)
export portable

# For Windows compilation
AC_ARG_ENABLE(Windows,
        [  --enable-Windows        Windows standalone options applied, maintainer only],
        Windows=$enableval, Windows="no"
)
export Windows
if test "$Windows" = "yes"; then
        portable="yes" ; export portable
fi

#AC_ARG_ENABLE( static,
#  [  --enable-static         Flag to force static compilation, default off],
#   static=$enableval, static="no"
# )
#export static
#AC_ARG_ENABLE( staticGCC,
#  [  --enable-staticGCC      Flag to force static gcc libraries, default off],
#    staticGCC=$enableval, staticGCC="no"
#  )
#export $staticGCC
#echo "HELLO" $staticGCC
# Debug mode (also turns on warnings)
AC_ARG_ENABLE(debug,
  [  --enable-debug          Flag to turn on debugging, default off],
   debug=$enableval, debug="no"
  )
WGCC="-w -fomit-frame-pointer"
WARN="-w"
WSGI="-woffall"
WALPA="-w0"
WHPUX="-w +O3 +Oall +Odataprefetch"
# +O3 breaks the C++ compile on HP, does not matter here at the moment
WHPCXX="-w +O2 +Oall"
WAIX="-w"
WSUN="-w -fast -O4"
WINTEL="-w -ip -axNB -O3 -mp -traceback -fminshared -scalar_rep"
WINTELC="-w -ip -axNB -O3 -mp"
if test "$debug" = "yes"; then
	WAIX="-g"
	WARN="-g"
	WSGI="-g"
	WHPUX="-g"
        WHPCXX="-g"
	WALPHA="-g"
# -fbounds-check breaks Owen's use of equivalences
#	WGCC="-Wall -g -fbounds-check"
        WGCC="-Wall -g"
	WSUN="-g"
#	WINTEL="-g -check -CB -f77rtl -traceback -W1"
       WINTEL="-g -check -CB -traceback "
       WINTELC="-g"
#	echo "Debugging has been turned on" >> edm.log
fi
export WARN WSGI WHPUX WALPHA WGCC WSUN WINTEL WINTELC

# Checks for programs.
ACX_PROG_CC_MAXOPT
ACX_PROG_F77_MAXOPT
dnl
dnl *********************Enable Integer 2 section ***********************
dnl 
AC_ARG_ENABLE( i2,
  [  --enable-i2             Adds -i2 option for integer*2 compilation, default off],
    oki2=$enableval, oki2=no )
 AC_ARG_ENABLE( I2,
  [  --enable-I2             Adds -I2 option for integer*2 compilation, default off],
    okI2=$enableval, okI2=no )
AC_ARG_ENABLE( fort77,
  [  --enable-fort77         Adds fort77 options with integer*2, default off],
    ok77=$enableval, ok77=no  )

if test "$ok77" = "no" && test "$oki2" = "no" && test "$okI2" = "no" ; then
#Default: use integer 4 version
        cp Includes/PARAMS_i4 Includes/PARAMS
        cp Includes/params.h_i4 Includes/params.h
        cp Includes/ftypes.h_i4 Includes/ftypes.h
        cp Primitives/mctime_test_i4.f Primitives/mctime_test.f
        cp Primitives/mcdc61_test.f_i4 Primitives/mcdc61_test.f
fi

if test "$oki2" = "yes" ; then
        AC_DEFINE(HAVE_i2_H,,
        [Defined to one if the compiler should use -i2 option])
        cp Includes/PARAMS_i2   Includes/PARAMS
        cp Includes/params.h_i2 Includes/params.h
        cp Includes/ftypes.h_i2 Includes/ftypes.h
        cp Primitives/mctime_test_i2.f Primitives/mctime_test.f
        cp Primitives/mcdc61_test.f_i2 Primitives/mcdc61_test.f
        FFLAGS="$FFLAGS -i2"
        AC_MSG_NOTICE([Adding -i2 to fortran compilation options])
fi

if test "$okI2" = "yes" ; then
        AC_DEFINE(HAVE_I2_H,,
        [Defined to one if the compiler should use -I2 option])
        cp Includes/PARAMS_i2   Includes/PARAMS
        cp Includes/params.h_i2 Includes/params.h
        cp Includes/ftypes.h_i2 Includes/ftypes.h
        cp Primitives/mctime_test_i2.f Primitives/mctime_test.f
        cp Primitives/mcdc61_test.f_i2 Primitives/mcdc61_test.f
        FFLAGS="$FFLAGS -I2"
        AC_MSG_NOTICE([Adding -I2 to fortran compilation options])
fi

if test "$ok77" = "yes" ; then
        AC_DEFINE(HAVE_fort77_H,,
        [Defined to one if using fort77 with -I2 and other options])
        cp Includes/PARAMS_i2 Includes/PARAMS
        cp Includes/params.h_i2 Includes/params.h
        cp Includes/ftypes.h_i2 Includes/ftypes.h
        cp Primitives/mctime_test_i2.f Primitives/mctime_test.f
        cp Primitives/mcdc61_test.f_i2 Primitives/mcdc61_test.f
        FFLAGS="$FFLAGS -I2 -Nn1604 -Nx400 -v -a -R"
        AC_MSG_NOTICE([Adding using fort77 compilation options])
# access and getcwd are not in f2c
        AC_LIBOBJ(accessf)
        AC_LIBOBJ(getcwdf)
        PATCHES="accessf.c+getcwdf.c"
fi

# Patches for gfortran
if test "$F77" = "gfortran" ; then
        AC_DEFINE(HAVE_gfortran_H,,
        [Defined to one if using fort77 with -I2 and other options])
        AC_LIBOBJ(accessf)
        PATCHES="accessf.c"
	FFLAGS="$FFLAGS -std=legacy -fallow-argument-mismatch"
fi
dnl
dnl *************************************************
dnl


# We seem to need C++ for fortran tests (?)
ACX_PROG_CXX_MAXOPT
AC_F77_LIBRARY_LDFLAGS
# Check wrappers
AC_F77_WRAPPERS
AC_F77_MAIN
#These appear to be for C (?)
#AC_FC_WRAPPERS

# Push C to the top for subsequent tests
ACX_PROG_CC_MAXOPT
AC_PROG_RANLIB

# Checks for header files.
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_CHECK_HEADERS([fcntl.h malloc.h memory.h stdlib.h string.h sys/file.h sys/param.h sys/time.h unistd.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_TIME
AC_STRUCT_TM

# Checks for library functions.
AC_FUNC_FORK
dnl Remove since it is broken in some makefiles
dnl AC_FUNC_MALLOC
AC_FUNC_MMAP
AC_FUNC_REALLOC
AC_TYPE_SIGNAL
AC_CHECK_FUNCS([ftruncate getpagesize gettimeofday memset munmap fseek])
# Check endianness
AC_C_BIGENDIAN


# Checks for libraries.
# Make sure that the math library is included
AC_CHECK_LIB(m , sin)
# Extra paths for X
AC_PATH_X
if test "$no_x" = "yes"; then
	echo "I cannot find a working X Window System"
	echo "You may need to set --x-includes=dir and --x-libraries=dir"
	echo "Giving up........sorry"
	exit -1
fi

AC_PATH_XTRA

dnl This is probably not needed....
AC_SUBST(X_LIBS)
AC_SUBST(X_PRE_LIBS)
AC_SUBST(X_CFLAGS)

LIBS="$X_LIBS $X_PRE_LIBS -lX11 $X_EXTRA_LIBS $LIBS"

dnl Check for CGWIN
CHECK_WITH_CYGWIN

# We need to make certain that X_CFLAGS is added to include path
CPPFLAGS="$X_CFLAGS $CPPFLAGS -I/usr/local/include -L/usr/local/lib -I${prefix}/include -L${prefix}/lib"

# what is limage ???
# FIXME: Replace `main' with a function in `-limage':
# AC_CHECK_LIB([image], [main])
# This to enable SGI intrinsics

dnl *************************************************
# Look for default focus mode
# Leave this unset for many cases, with a user over-rider included
AC_ARG_ENABLE(focus,
  [  --enable-focus          Makes click-to-focus default mode (bad for Windows)
  			  Better for most other systems - not implimented yet],
   ok=$enableval, ok=no
  )
if test "$ok" = "yes" ; then
                AC_DEFINE([HAVE_CLICK_TO_FOCUS],,[Determine the focus mode])
fi

dnl *************************************************
# There are some issues with MMAP in dcache, probably wrong calls
# Define to use malloc for the cache by default, otherwise mmap
AC_ARG_ENABLE(mmap,
  [  --enable-mmap           Use mmap for the cache (dangerous) ],
   ok=$enableval, ok=no
  )

if test "$ok" = "yes" ; then
                AC_DEFINE([CACHE_MMAP],,[Mmap used for Cache])
else
                AC_DEFINE([CACHE_MALLOC],,[Malloc used for Cache])
fi

dnl *************************************************

# Find a Browser
echo "Trying to find a reasonable browser for help"
# Do We have Cygwin
case $host_os in 
	*cygwin* )      CYGWIN=yes;; 
	* )             CYGWIN=no;; 
esac 

# Find a Browser unless cygwin is set
echo "Trying to find a reasonable browser for help"
if test "$CYGWIN" = "no" ; then
	AC_FIND_BROWSER
else
	BROWSER="cygshelp.sh"
	BROWSERF="cygshelp.sh"
	# AC_DEFINE(HAVE_CYGWIN,,Defined if Cygwin compilation)
fi

# Include directory
        FFLAGS="$FFLAGS -I../Includes"
        CFLAGS="$CFLAGS -I../Includes"
        export FFLAGS CFLAGS
# Add Libraries. Do it many times to catch order problems
        SLIB_ADD1="-lsPrimitives -lsCommands -lsLocals -lsMiddle -lsProgramM"
        SLIB_ADD2="-lsPatches -lsUserInterface -lsGraphics -lsKernel"
        SLIB_ADD="$SLIB_ADD1 $SLIB_ADD2 $SLIB_ADD1 $SLIB_ADD2"
        AC_SUBST(SLIB_ADD)
        SLIB_LOC="-L../Commands -L../Kernel -L../Middle -L../ProgramM"
        SLIB_LOC="$SLIB_LOC -L../UserInterface -L../Graphics -L../Primitives"
        SLIB_LOC="$SLIB_LOC -L../Locals -L../Patches"
        AC_SUBST(SLIB_LOC)

# Find patches that are needed (?)
        AC_CONFIG_LIBOBJ_DIR(Patches)
        AC_LANG([C])
# Any C code - nothing for the moment
#
        AC_LANG([Fortran 77])
# Look for usleep
        AC_LINK_IFELSE( AC_LANG_PROGRAM([],
[       INTEGER*4 IMSEC
	IMSEC=1000
        CALL USLEEP(IMSEC)]),
        [usleep="yes"], [usleep="no"] )
	
	if test "$usleep" = "no" ; then
		AC_LIBOBJ(usleep)
                PATCHES="$PATCHES+usleep.c"
		AC_MSG_NOTICE([Adding usleep.c to the installation])
	fi

# Look for lnblnk, or len_trim if it is not found
        AC_LIBSOURCE([lnblnk1.f])
        AC_LINK_IFELSE( AC_LANG_PROGRAM([],
[       i=LNBLNK('Test  ')
        j=2]),
        [lnblnk="yes"], [lnblnk="no"] )

        AC_LINK_IFELSE( AC_LANG_PROGRAM([],
[       i=LEN_TRIM('Test  ')
        j=2]),
        [ltrim="yes"], [ltrim="no"] )

        if test "$lnblnk" = "yes" ; then
                AC_MSG_NOTICE([Fortran has lnblnk intrinsic])
        else
                AC_MSG_NOTICE([Fortran needs lnblnk intrinsic])
        fi

        if test "$lnblnk" = "no" ; then
		if test "ltrim" = "yes" ; then
# Use the Fortran version
			lnblnk1="lnblnk1.f"
                	AC_LIBOBJ([[$lnblnk1]])
                	PATCHES="$PATCHES+lnblnk1.f"
                	AC_MSG_NOTICE([Adding lnblnk1.f to the installation])
		else
# Use the C version
			AC_LIBOBJ(lnblnk)
        	fi
	fi
# How is fseek compiled

        AC_LINK_IFELSE( AC_LANG_PROGRAM([],
[        integer*4 FSEEK
         integer*4  I1
         integer*4  I2
         integer*4  I3
        i=FSEEK( I1, I2, I3) ]),
        [lfseek="filpos_v1"], [lfseek="filpos_v2"] )		
        AC_MSG_NOTICE([Version of filpos used is $lfseek])
        AC_LIBSOURCES([filpos_v1.f, filpos_v2.f])
	AC_LIBOBJ([[$lfseek]])
        PATCHES="$PATCHES+$lfseek"
	AC_SUBST(PATCHES)
        BTIME="$(date | sed -e 's/:/-/g')"
        AC_SUBST(BTIME)        
AC_CONFIG_FILES(
                Makefile
                Commands/Makefile
                Graphics/Makefile
                Kernel/Makefile
                Middle/Makefile
                ProgramM/Makefile
                UserInterface/Makefile
                Primitives/Makefile
                Locals/Makefile
                Main/Makefile
                Includes/Makefile
                Demo/Makefile
                Examples/Makefile
                Patches/Makefile
                Test/Makefile
)
AC_OUTPUT
#Simple message at end
        echo " "
        echo "To compile, type in:"
        echo "make"
        echo "make install"
        echo "make help"
