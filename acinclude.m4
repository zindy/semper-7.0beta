dnl Find out if we have an appropriate version of fftw
dnl

AC_DEFUN([ACX_FFTW_STATUS],
[

dnl Checks in libraries for sin - needed!.
#AC_CHECK_LIB(m, sin)
ok="no"
fftw="no"
sfftw="no"

# Do we have sfftw.h ?
AC_CHECK_HEADERS([sfftw.h],
                [sfftw="yes"],
                [sfftw="no"])
# Yes, check that we can compile against it
if test "$sfftw" = "yes" ; then
   ok="yes"
   AC_CHECK_LIB(sfftw,fftw_create_plan,,
        [
           echo "";
           echo "Alas, the sfftw library does not seem to work";
           echo "(I might be in trouble now, not sure yet)";
           echo "";
	   ok="no"
	  ],[-lm]) 
fi

dnl If we don't have a viable sfftw
if test "$sfftw" = "no" ; then

	AC_CHECK_HEADERS([fftw.h],
                	[fftw="yes"],
                	[fftw="no"])
else
	AC_DEFINE(HAVE_SFFTW_H)
fi


if test "$fftw" = "yes" ; then
# Found fftw, see if it is single precision
	  ok="yes"
        AC_RUN_IFELSE(
        AC_LANG_PROGRAM(
        [[#include <fftw.h>]],
        [[#ifndef FFTW_ENABLE_FLOAT
        exit(-1);
        #endif]]),
	[echo "Checking for single-precision fftw...ok"],
	[
        echo "";
        echo "Found a double-precision fftw, but I need single-precision";
        echo "(I might be in trouble now, not sure yet)";
        echo "";
 	fftw="no"
        ok="no"
        ],
        [
        echo "";
        echo "We appear to be cross-compiling, check compiled code";
        echo "(I might be in trouble now)";
        echo "";
	fftw="no"
        ok="no"
        ])
fi
if test "$fftw" = "yes" ; then
# Test a little more carefully
        ok="yes"
        AC_CHECK_LIB(fftw,fftw_create_plan,,
        [
         echo "";
         echo "Alas, fftw library does not seem to work";
         echo "I need to have this library to go further";
         echo "(I might be in trouble now, not sure yet)";
         echo "";
         ok="no"
        ],[-lm])
fi

if test "$fftw" = "yes" ; then
	AC_DEFINE(HAVE_FFTW_H)
fi

if test "$ok" = "no" ; then
         echo "";
	echo "Cannot find a working fftw library or header";
        echo "This is not a problem, but I need to have one";
        echo "to compile the edm software";
        echo "";
        echo "If you want to seperately compile the code,";
        echo "Answer no below (which will stop configure)";
	AC_PROMPT_USER([fftw],
       ["Do you want me to compile fftw"],
       ["yes"])	
fi
if test "$fftw" != "yes" && test "$sfftw" != "yes" ; then
        echo "";
	echo "Not much I can do, so I'm giving up, sorry";
	echo "If you want to force compilation, use --enable-fftw or --enable-both";
	echo "when you rerun ./configure";
	echo "Otherwise compile forms yourself or answer yes next time";
	exit -1
fi

dnl If we get here and $ok is no, we should compile fftw here
dnl Use fftw for this purpose
if test "$ok" = "yes" ; then
	fftw="no"
fi
]
)dnl
dnl Find a browser, exporting a full and partial path
dnl

AC_DEFUN([AC_FIND_BROWSER],

[
AC_PATH_PROGS(BROWSER, 
        [opera Opera netscape Netscape Netscp mozilla IEXPLORE Iexplore] 
        ,[None],  
        [$PATH:/usr/local/bin:/usr/share/bin:/opt/netscape/bin]
        ) 
AC_CHECK_PROGS(BROWSERF, 
        [opera Opera  mozilla netscape Netscape Netscp IEXPLORE Iexplore] 
        ,[None],  
        [$PATH:/usr/local/bin:/usr/share/bin:/opt/netscape/bin]
        ) 

dnl Some more linux type stuff
if test "$BROWSER" = "None" ; then
AC_PATH_PROGS(BROWSER,
        [Mozilla mozilla htmlview]
        ,[None],
        [$PATH:/usr/local/bin:/usr/share/bin:/opt/netscape/bin:/usr/bin]
        )
AC_CHECK_PROGS(BROWSERF,
        [Mozilla mozilla htmlview]
        ,[None],
        [$PATH:/usr/local/bin:/usr/share/bin:/opt/netscape/bin:/usr/bin]
        )
fi

dnl Try some MAC-like stuff
if test "$BROWSER" = "None" ; then
AC_PATH_PROGS(BROWSER, 
        [Safari firefox] 
        ,[None],  
        [$PATH:/Applications/Safari.apps/Contents/MacOs/Safari:/Applications/Safari.apps/Contents/MacOs/firefox]
        )
AC_CHECK_PROGS(BROWSERF, 
        [Safari firefox] 
        ,[None],  
        [$PATH:/Applications/Safari.apps/Contents/MacOs/Safari:/Applications/Safari.apps/Contents/MacOs/firefox]
        )
fi

dnl Try some cygwin-DOS stuff
if test "$BROWSER" = "None" ; then
PATH=$PATH:/cygdrive/c/PROGRA~1/INTERN~1:/cygdrive/c/PROGRA~1/Netscape:/cygdrive/c/PROGRA~1/MOZILL~1
export PATH
AC_PATH_PROGS(BROWSER, 
        [firefox firefox.exe iexplore.exe iexplore Opera.exe Netscape.exe Netscape Communicator.exe mozilla.exe] 
        ,[None],  
        [$PATH]
        ) 
AC_CHECK_PROGS(BROWSERF, 
        [firefox firefox.exe iexplore.exe iexplore Opera.exe Netscape.exe Netscape Communicator.exe mozilla.exe] 
        ,[None],  
        [$PATH]
        ) 
fi



AC_SUBST(BROWSER)
AC_SUBST(BROWSERF)

])
dnl Find out if we have an appropriate version of forms
dnl
AC_DEFUN([ACX_FORMS_STATUS],
[
# Ensure that we are using C!
AC_LANG([C])
# Need Math libraries
AC_CHECK_LIB(m, sin)
# Do we have forms.h ?
ok="no"
AC_CHECK_HEADERS([forms.h],
                [forms="yes"],
                [forms="no"])
# Yes, check that we can compile against it
if test "$forms" = "yes" ; then
	ok="yes"
    	AC_SEARCH_LIBS([fl_set_focus_object],forms form,,
        [echo "";
         echo "The forms library does not seem to work";
         echo "I need to have this library to go further";
         echo "(I might be in trouble now, not sure yet)";
         echo "";
	   ok="no";
	  ],[-lX11])
	if test "$ok" = "yes" ; then
		echo "Existing forms library appears to be good";
	fi
fi

dnl If forms does not work or not found
if test "$ok" = "no" ; then

         	echo "";
		echo "Cannot find a working forms library or header" ;
        	echo "This is not a problem, but I need to have this";
        	echo "to compile the edm software";
         	echo "";
        	echo "If you want to seperately compile the code,";
        	echo "Answer no below (which will stop configure)";
		AC_PROMPT_USER([forms],
		   ["Do you want me to compile forms"],
		   ["yes"])
fi
if test "$forms" != "yes" ; then
         	echo "";
		echo "Not much I can do, so I'm giving up, sorry";
		echo "If you want to force compilation, use --enable-forms or --enable-both";
		echo "Otherwise compile forms yourself or answer yes next-time";
		exit -1;
fi

dnl If we get here and $ok is no, we should compile forms here
dnl Use forms for this purpose
if test "$ok" = "yes" ; then
	forms="no"
fi

]
)
dnl

 
AC_DEFUN(
        [CHECK_GNU_MAKE], [ AC_CACHE_CHECK( for GNU make,_cv_gnu_make_command,
                _cv_gnu_make_command='' ;
dnl Search all the common names for GNU make
                for a in "$MAKE" make gmake gnumake ; do
                        if test -z "$a" ; then continue ; fi ;
                        if  ( sh -c "$a --version" 2> /dev/null | grep GNU  2>&1 > /dev/null ) ;  then
                                _cv_gnu_make_command=$a ;
                                break;
                        fi
                done ;
        ) ;
dnl If there was a GNU version, then set @ifGNUmake@ to the empty string, 'No' otherwise
        if test  "x$_cv_gnu_make_command" != "x"  ; then
                ifGNUmake='' ;
        else
                ifGNUmake='No' ;
                AC_MSG_RESULT("Not found");
		echo "I did not find a GNU make" >> edm.log ;
		echo "You might run into problems during the make phase" >> edm.log ;
        fi
dnl        AC_SUBST(ifGNUmake)
] )
dnl Modified macro to ask a question
AC_DEFUN([AC_PROMPT_USER],
[
echo $ECHO_N "$2 (for affirmative, enter $3) $ECHO_C"
tmpinput=""
read tmpinput
eval $1=$tmpinput
]
) dnl

AC_DEFUN([ACX_CHECK_CC_FLAGS],
[
AC_REQUIRE([AC_PROG_CC])
ac_save_CFLAGS=$CFLAGS
CFLAGS="$1"
AC_CACHE_CHECK(whether $CC accepts $1, ac_cv_$2,
               [AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], [ac_cv_$2=yes], 
						       [ac_cv_$2=no])])
CFLAGS=$ac_save_CFLAGS
if test "$ac_cv_$2" = yes; then
	:
	$3
else
	:
	$4
fi
])

AC_DEFUN([ACX_PROG_CC_MAXOPT],
[
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([ACX_PROG_CC_EGCS])
AC_REQUIRE([AC_CANONICAL_HOST])

# Try to determine "good" native compiler flags if none specified on command
# line
echo $ac_test_CFLAGS
if test "$ac_test_CFLAGS" != "set"; then
  CFLAGS=""
  goticc="no"
  case "${host_cpu}-${host_os}" in

  *linux*) if test "$CC" = icc -o "$CC" = icpc ; then
                    CFLAGS="$WINTELC -cxxlib-icc -norestrict -cxxlib-icc -lcxa -lcxaguard -lunwind "
#                    CFLAGS="-cxxlib-icc -norestrict -cxxlib-icc -lcxa -lcxaguard -lunwind "
                    goticc="yes"
	        fi;;
  sparc-solaris2*) if test `basename "$CC"` = cc; then
                    CFLAGS="-native -fast -xO5 -dalign $WSUN"
                 fi;;

  alpha*-osf*)  if test "$CC" = cc; then
                    CFLAGS="-newc $WALPH -O5 -ansi_alias -ansi_args -fp_reorder -tune host -arch host"
                fi;;

  hppa*-hpux*)  if test "$ac_compiler_gnu" != yes; then
                    CFLAGS="-Ae $WHPUX"
                    LDFLAGS="$LDFLAGS -L/usr/local/lib"
                fi;;

  *sgi-irix*)	if test "$CC" = cc; then
		    CFLAGS="-O3 $WSGI"
		fi;;

   *-aix*)
	if test "$CC" = cc -o "$CC" = xlc -o "$CC" = ccc; then
                ACX_CHECK_CC_FLAGS(-qarch=auto $WAIX -qtune=auto, qarch_auto,
                        CFLAGS="-O3 -qansialias $WAIX -qarch=auto -qtune=auto",
                        [CFLAGS="-O3 -qansialias $WAIX"
                echo "*******************************************************">> edm.log
                echo "*  You seem to have AIX and the IBM compiler.  It is  *">> edm.log
                echo "*  recommended for best performance that you use:     *">> edm.log
                echo "*                                                     *">> edm.log
                echo "*    CFLAGS=-O3 -qarch=xxx -qtune=xxx -qansialias -w  *">> edm.log
		echo "*    Similarly for CXXFLAGS & FFLAGS                  *">> edm.log
                echo "*                      ^^^        ^^^                 *">> edm.log
                echo "*  where xxx is pwr2, pwr3, 604, or whatever kind of  *">> edm.log
                echo "*  CPU you have.  (Set the CFLAGS environment var.    *">> edm.log
                echo "*  and re-run configure.)  For more info, man cc.     *">> edm.log
                echo "*******************************************************">> edm.log
		]
                )
        fi;;
  esac

  # use default flags for gcc on all systems
  if test $ac_cv_prog_gcc = yes && test "$goticc" = "no" ; then
     CFLAGS="$WGCC"
  fi

  # the egcs scheduler is too smart and destroys our own schedule.
  # Disable the first instruction scheduling pass.  The second
  # scheduling pass (after register reload) is ok.
  if test "$acx_prog_egcs" = yes; then
     if test "$1" = fftw; then
        CFLAGS="$CFLAGS -fno-schedule-insns"
     fi
  fi

# test for gcc-specific flags:
# if test $ac_cv_prog_gcc = yes && test "$goticc" = "no" ; then
#     -malign-double for x86 systems
#    ACX_CHECK_CC_FLAGS(-malign-double,align_double,
#	CFLAGS="$CFLAGS -malign-double")
#     -fstrict-aliasing for gcc-2.95+, don't use
#    ACX_CHECK_CC_FLAGS(-fstrict-aliasing,fstrict_aliasing,
#	CFLAGS="$CFLAGS -fstrict-aliasing")
#    ACX_CHECK_CC_FLAGS(-mpreferred-stack-boundary=4, m_psb_4,
#        CFLAGS="$CFLAGS -mpreferred-stack-boundary=4")
# fi

# test if the compiler accepts static
  if test "$static" = "yes"; then
    ACX_CHECK_CC_FLAGS(-static, m_static_cc,CFLAGS="$CFLAGS -static")
  fi

  CPU_FLAGS=""
  CPU_OPTIM=""
  ACX_CHECK_CC_FLAGS(-march=native,cpu_native,
                        [CPU_FLAGS=-march=native])
  CODELET_OPTIM=""
  if test "$GCC" = "yes" && test "$goticc" = "no"; then
         if  test "$goticc" = "no" ; then
                  CPU_OPTIM=-O3
                  CODELET_OPTIM=-O3
          fi
	  dnl try to guess correct CPU flags, at least for linux
	  case "${host_cpu}" in
	  i586*)  ACX_CHECK_CC_FLAGS(-march=pentium,cpu_pentium,
			[CPU_FLAGS=-march=pentium],
			[ACX_CHECK_CC_FLAGS(-mpentium,pentium,
				[CPU_FLAGS=-mpentium])])
		  if test "$1" = fftw; then
	            CODELET_OPTIM=-O
                  fi
		  if test "$1" = benchfft; then
	            CPU_OPTIM=-O2
                  fi
		  ;;
	  i686*)  ACX_CHECK_CC_FLAGS(-march=native,cpu_native,
			[CPU_FLAGS=-march=native])
		  if test "$1" = fftw; then
	            CODELET_OPTIM=-O
                  fi
		  if test "$1" = benchfft; then
	            CPU_OPTIM=-O2
                  fi
		  ;;
	  sparc*)  
dnl  Removed          ACX_CHECK_CC_FLAGS(-march=ultrasparc,cpu_ultrasparc,
dnl			[CPU_FLAGS=-march=ultrasparc])
		  ;;
	  alphaev67)  ACX_CHECK_CC_FLAGS(-march=ev67,cpu_ev67,
			[CPU_FLAGS=-march=ev67])
		  ;;
	  alphaev6)  ACX_CHECK_CC_FLAGS(-march=ev6,cpu_ev6,
			[CPU_FLAGS=-march=ev6])
		  ;;
	  alphaev56)  ACX_CHECK_CC_FLAGS(-march=ev56,cpu_ev56,
			[CPU_FLAGS=-march=ev56],
			[ACX_CHECK_CC_FLAGS(-march=ev5,cpu_ev5,
				[CPU_FLAGS=-march=ev5])])
		  ;;
	  alphaev5)  ACX_CHECK_CC_FLAGS(-march=ev5,cpu_ev5,
			[CPU_FLAGS=-march=ev5])
		  ;;
             hppa*)  ACX_CHECK_CC_FLAGS(-march=2.0,,
                        [CPU_FLAGS=-march=2.0])          
                  ;;
	  powerpc*)
		cputype=`((grep cpu /proc/cpuinfo | head -1 | cut -d: -f2 | sed 's/ //g') ; /usr/bin/machine ; /bin/machine) 2> /dev/null`
		cputype=`echo $cputype | sed -e s/ppc//g`
		is60x=`echo $cputype | egrep "^60[[0-9]]e?$"`
		is750=`echo $cputype | grep "750"`
		is74xx=`echo $cputype | egrep "^74[[0-9]][[0-9]]$"`
		if test -n "$is60x"; then
			ACX_CHECK_CC_FLAGS(-march=$cputype,m_cpu_60x,
				CPU_FLAGS=-march=$cputype)
		elif test -n "$is750"; then
			ACX_CHECK_CC_FLAGS(-march=750,m_cpu_750,
				CPU_FLAGS=-march=750)
		elif test -n "$is74xx"; then
			ACX_CHECK_CC_FLAGS(-march=$cputype,m_cpu_74xx,
				CPU_FLAGS=-march=$cputype)
		fi
		if test -z "$CPU_FLAGS"; then
		        ACX_CHECK_CC_FLAGS(-march=powerpc,m_cpu_powerpc,
				CPU_FLAGS=-march=powerpc)
		fi
		if test -z "$CPU_FLAGS"; then
			ACX_CHECK_CC_FLAGS(-mpowerpc,m_powerpc,
				CPU_FLAGS=-mpowerpc)
		fi
	  esac
# Added material if wincpu has been defined
        echo Testing for PC specific option -- $wincpu 
	if test "$wincpu" != "None"; then
	   CPU_FLAGS="-march=$wincpu"
	   case "$wincpu" in
		portable)
	   		ACX_CHECK_CC_FLAGS(-mfpmath=387,math_387,
			[CPU_FLAGS="$CPU_FLAGS -mfpmath=387"])
			;;			  	   		
	   	i586)
	   		ACX_CHECK_CC_FLAGS(-march=i586,cpu_i586,
			[CPU_FLAGS=-march=i586])
			;;			  	   
	   	pentium)
	   		ACX_CHECK_CC_FLAGS(-march=pentium,cpu_pentium,
			[CPU_FLAGS=-march=pentium])
			;;			  
	   	pentium2)
	   		ACX_CHECK_CC_FLAGS(-march=pentium2,cpu_pentium2,
			[CPU_FLAGS=-march=pentium2])
	   		ACX_CHECK_CC_FLAGS(-mmmx,mmmx,
			[CPU_FLAGS="$CPU_FLAGS -mmmx"])
			;;
	   	pentium3)
	   		ACX_CHECK_CC_FLAGS(-march=pentium3,cpu_pentium3,
			[CPU_FLAGS=-march=pentium3])
	   		ACX_CHECK_CC_FLAGS(-mmmx,mmmx,
			[CPU_FLAGS="$CPU_FLAGS -mmmx"])
			;;			  
	   	pentium4)
	   		ACX_CHECK_CC_FLAGS(-march=pentium4,cpu_pentium4,
			[CPU_FLAGS=-march=pentium4])
                        ACX_CHECK_CC_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CC_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CC_FLAGS(-mfpmath,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	pentium4m)
	   		ACX_CHECK_CC_FLAGS(-march=pentium4m,cpu_pentium4m,
			[CPU_FLAGS=-march=pentium4m])
	   		ACX_CHECK_CC_FLAGS(-msse,msse,
			[CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CC_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CC_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	prescott)
	   		ACX_CHECK_CC_FLAGS(-march=prescott,cpu_prescott,
			[CPU_FLAGS=-march=prescott])
	   		ACX_CHECK_CC_FLAGS(-msse,msse,
			[CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CC_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CC_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	Xeon*)
	   		ACX_CHECK_CC_FLAGS(-march=pentium4,cpu_pentium4,
			[CPU_FLAGS=-march=pentium4])
                        ACX_CHECK_CC_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CC_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CC_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			
                core2)
                        ACX_CHECK_CC_FLAGS(-mtune=core2,cpu_core2,[CPU_FLAGS="-mtune=core2"])
                        ;;  
	   	k6)
	   		ACX_CHECK_CC_FLAGS(-march=k6,cpu_k6,
			[CPU_FLAGS=-march=k6])
			;;			  
	   	athlon)
	   		ACX_CHECK_CC_FLAGS(-march=athlon,cpu_athlon,
			[CPU_FLAGS=-march=athlon])
	   		ACX_CHECK_CC_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   	athlon-xp)
	   		ACX_CHECK_CC_FLAGS(-march=athlon-xp,cpu_athlon-xp,
			[CPU_FLAGS=-march=athlon-xp])
	   		ACX_CHECK_CC_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   	Opteron*)
	   		ACX_CHECK_CC_FLAGS(-march=athlon,cpu_athlon,
			[CPU_FLAGS=-march=athlon])
	   		ACX_CHECK_CC_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   esac
	else
  		dnl More recent version
  		AX_GCC_ARCHFLAG([$portable], [CPU_FLAGS=$ax_cv_gcc_archflag],[echo "Did not find gcc flags"])
	fi
  fi


  if test -n "$CPU_OPTIM"; then
        CFLAGS="$CPU_OPTIM $CFLAGS"
  fi

  if test -n "$CPU_FLAGS"; then
        CFLAGS="$CFLAGS $CPU_FLAGS"
  fi

  AC_SUBST(CODELET_OPTIM)

  if test -z "$CFLAGS"; then
	echo "" >> edm.log
	echo "********************************************************" >> edm.log
        echo "* WARNING: Don't know the best CFLAGS for this system  *" >> edm.log
        echo "* Use  make CFLAGS=..., or edit the top level Makefile *" >> edm.log
	echo "* (otherwise, a default of CFLAGS=-O3 will be used)    *" >> edm.log
	echo "********************************************************" >> edm.log
	echo "" >> edm.log
        CFLAGS="-O3"
  fi

  ACX_CHECK_CC_FLAGS(${CFLAGS}, guessed_cflags, , [
	echo "" >> edm.log
        echo "********************************************************" >> edm.log
        echo "* WARNING: The guessed CFLAGS don't seem to work with  *" >> edm.log
        echo "* your compiler.                                       *" >> edm.log
        echo "* Use  make CFLAGS=..., or edit the top level Makefile *" >> edm.log
        echo "********************************************************" >> edm.log
        echo "" >> edm.log
        CFLAGS=""
  ])

fi
])
AC_DEFUN([ACX_CHECK_CXX_FLAGS],
[
AC_REQUIRE([AC_PROG_CXX])
ac_save_CXXFLAGS=$CXXFLAGS
CXXFLAGS="$1"
AC_CACHE_CHECK(whether $CXX accepts $1, ac_cv_$2,
               [AC_LINK_IFELSE([AC_LANG_PROGRAM()], [ac_cv_$2=yes], 
				[ac_cv_$2=no],	    [ac_cv_$2=no])])
CXXFLAGS=$ac_save_CXXFLAGS
if test "$ac_cv_$2" = yes; then
	:
	$3
else
	:
	$4
fi
])

AC_DEFUN([ACX_PROG_CXX_MAXOPT],
[
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_PROG_CXX])
AC_REQUIRE([ACX_PROG_CC_EGCS])
AC_REQUIRE([AC_CANONICAL_HOST])

# Try to determine "good" native compiler flags if none specified on command
# line
AC_LANG([C++])
if test "$ac_test_CXXFLAGS" != "set"; then
  CXXFLAGS=""
  goticc="no"
  case "${host_cpu}-${host_os}" in

  *linux*) if test "$CXX" = icc -o "$CXX" = icpc ; then
                    CXXFLAGS="$WINTELC -cxxlib-icc -norestrict -cxxlib-icc -lcxa -lcxaguard -lunwind"
#                    CXXFLAGS="-cxxlib-icc -norestrict -cxxlib-icc -lcxa -lcxaguard -lunwind"
                    goticc="yes"
	        fi;;
  sparc-solaris2*) if test `basename "$CC"` = cc; then
                    CXXFLAGS="-native -fast -xO5 -dalign $WSUN -DSUN"
                 fi;;

  alpha*-osf*)  if test "$CC" = cc; then
                    CXXFLAGS="-newc $WALPHA -O5 -ansi_alias -ansi_args -fp_reorder -tune host -arch host -DALPHA"
                fi;;

  hppa*-hpux*)  if test "$ac_compiler_gnu" != yes; then
                    CXXFLAGS="-Aa $WHPUX -DHPUX"
                fi;;

  *-sgi-irix*)	if test "$CC" = cc; then
		    CXXFLAGS="-O3 $WSGI -DSGI"
		fi;;

   *-aix*)
	if test "$CC" = cc -o "$CC" = xlc -o "$CC" = ccc; then
                ACX_CHECK_CXX_FLAGS(-qarch=auto $WAIX -qtune=auto, qarch_auto,
                        [CXXFLAGS="-O3 -qansialias $WAIX -qarch=auto -qtune=auto"]
                        [CXXFLAGS="-O3 -qansialias $WAIX"
                echo "*******************************************************"
                echo "*  You seem to have AIX and the IBM compiler.  It is  *"
                echo "*  recommended for best performance that you use:     *"
                echo "*                                                     *"
                echo "*  CXXFLAGS=-O3 -qarch=xxx -qtune=xxx -qansialias -w  *"
                echo "*                      ^^^        ^^^                 *"
                echo "*  where xxx is pwr2, pwr3, 604, or whatever kind of  *"
                echo "*  CPU you have.  (Set the CXXFLAGS environment var.  *"
                echo "*  and re-run configure.)  For more info, man cc.     *"
                echo "*******************************************************"])
		CXXFLAGS="$CXXFLAGS -DIBM"
        fi;;
  esac

  # use default flags for gcc on all systems
  if test $ac_cv_prog_gcc = yes  && test "$goticc" = "no"; then
     CXXFLAGS="$WGCC"
  fi

  # the egcs scheduler is too smart and destroys our own schedule.
  # Disable the first instruction scheduling pass.  The second
  # scheduling pass (after register reload) is ok.
  if test "$acx_prog_egcs" = yes; then
     if test "$1" = fftw; then
        CXXFLAGS="$CXXFLAGS -fno-schedule-insns"
     fi
  fi

  # test for gcc-specific flags:
  if test $ac_cv_prog_gcc = yes && test "$goticc" = "no"; then
#     -malign-double for x86 systems
#   ACX_CHECK_CXX_FLAGS(-malign-double,align_double,
#	CXXFLAGS="$CXXFLAGS -malign-double")
#     -fstrict-aliasing for gcc-2.95+
#    ACX_CHECK_CXX_FLAGS(-fstrict-aliasing,fstrict_aliasing,
#	CXXFLAGS="$CXXFLAGS -fstrict-aliasing")
#   ACX_CHECK_CXX_FLAGS(-mpreferred-stack-boundary=4,m_psb_4,
#       CXXFLAGS="$CXXFLAGS -mpreferred-stack-boundary=4")
    # -pipe ?
    ACX_CHECK_CXX_FLAGS(-pipe,pipe,
        CXXFLAGS="$CXXFLAGS -pipe")
    # -fpermissive
    ACX_CHECK_CXX_FLAGS(-fpermissive,permissive,
        CXXFLAGS="$CXXFLAGS -fpermissive")
    # prototypes
    # ACX_CHECK_CXX_FLAGS(-fno-strict-prototypes,prototypes,
    #   CXXFLAGS="$CXXFLAGS -fno-strict-prototypes")
    # null objects
    # ACX_CHECK_CXX_FLAGS(-fnonnull-objects,nonnullobjects,
    #    CXXFLAGS="$CXXFLAGS -fnonnull-objects")
  fi
# test if the compiler accepts static
  if test "$static" = "yes"; then
    ACX_CHECK_CXX_FLAGS(-static, m_static_cxx,CXXFLAGS="$CXXFLAGS -static")
  fi

  CPU_FLAGS=""
  ACX_CHECK_CXX_FLAGS(-march=native,cpu_native,
                        [CPU_FLAGS=-march=native])
  CPU_OPTIM=""
  CODELET_OPTIM=""
  if test "$GCC" = "yes" && test "$goticc" = "no" ; then
	  if  test "$goticc" = "no" ; then
                  CPU_OPTIM=-O3
        	  CODELET_OPTIM=-O3
          fi
	  dnl try to guess correct CPU flags, at least for linux
	  case "${host_cpu}" in
	  i586*)  ACX_CHECK_CXX_FLAGS(-march=pentium,cpu_pentium,
			[CPU_FLAGS=-march=pentium],
			[ACX_CHECK_CXX_FLAGS(-mpentium,pentium,
				[CPU_FLAGS=-mpentium])])
		  if test "$1" = fftw; then
	            CODELET_OPTIM=-O
                  fi
		  if test "$1" = benchfft; then
	            CPU_OPTIM=-O2
                  fi
		  ;;
	  i686*)  ACX_CHECK_CXX_FLAGS(-march=native,cpu_native,
			[CPU_FLAGS=-march=native])
		  if test "$1" = fftw; then
	            CODELET_OPTIM=-O
                  fi
		  if test "$1" = benchfft; then
	            CPU_OPTIM=-O2
                  fi
		  ;;
	  sparc*)
dnl                   ACX_CHECK_CXX_FLAGS(-march=ultrasparc,cpu_ultrasparc,
dnl			[CPU_FLAGS="-march=ultrasparc -DSUN"])
		  ;;
	  alphaev67)  ACX_CHECK_CXX_FLAGS(-march=ev67,cpu_ev67,
			[CPU_FLAGS="-march=ev67 -DALPHA"])
		  ;;
	  alphaev6)  ACX_CHECK_CXX_FLAGS(-march=ev6,cpu_ev6,
			[CPU_FLAGS="-march=ev6 -DALPHA"])
		  ;;
	  alphaev56)  ACX_CHECK_CXX_FLAGS(-march=ev56,cpu_ev56,
			[CPU_FLAGS="-march=ev56 -DALPHA"],
			[ACX_CHECK_CXX_FLAGS(-march=ev5,cpu_ev5,
				[CPU_FLAGS="-march=ev5 -DALPHA"])])
		  ;;
	  alphaev5)  ACX_CHECK_CXX_FLAGS(-march=ev5,cpu_ev5,
			[CPU_FLAGS="-march=ev5 -DALPHA"])
		  ;;
             hppa*)  ACX_CHECK_CXX_FLAGS(-march=2.0,,
                        [CPU_FLAGS="-march=2.0 -DHP"])          
                  ;;
	  powerpc*)
		cputype=`((grep cpu /proc/cpuinfo | head -1 | cut -d: -f2 | sed 's/ //g') ; /usr/bin/machine ; /bin/machine) 2> /dev/null`
		cputype=`echo $cputype | sed -e s/ppc//g`
		is60x=`echo $cputype | egrep "^60[[0-9]]e?$"`
		is750=`echo $cputype | grep "750"`
		is74xx=`echo $cputype | egrep "^74[[0-9]][[0-9]]$"`
		if test -n "$is60x"; then
			ACX_CHECK_CXX_FLAGS(-march=$cputype,m_cpu_60x,
				CPU_FLAGS=-march=$cputype)
		elif test -n "$is750"; then
			ACX_CHECK_CXX_FLAGS(-march=750,m_cpu_750,
				CPU_FLAGS=-march=750)
		elif test -n "$is74xx"; then
			ACX_CHECK_CXX_FLAGS(-march=$cputype,m_cpu_74xx,
				CPU_FLAGS=-march=$cputype)
		fi
		if test -z "$CPU_FLAGS"; then
		        ACX_CHECK_CXX_FLAGS(-march=powerpc,m_cpu_powerpc,
				CPU_FLAGS=-march=powerpc)
		fi
		if test -z "$CPU_FLAGS"; then
			ACX_CHECK_CXX_FLAGS(-mpowerpc,m_powerpc,
				CPU_FLAGS=-mpowerpc)
		fi
	  esac
# Added material if wincpu has been defined
	if test "$wincpu" != "None"  && test "$goticc" = "no"; then
	   CPU_FLAGS="-march=$wincpu"
	   case "$wincpu" in
		portable)
	   		ACX_CHECK_CXX_FLAGS(-mfpmath=387,math_387,
			[CPU_FLAGS="$CPU_FLAGS -mfpmath=387"])
			;;			  	   		
	   	i586)
	   		ACX_CHECK_CXX_FLAGS(-march=i586,cpu_i586,
			[CPU_FLAGS=-march=i586])
			;;			  	   
	   	pentium)
	   		ACX_CHECK_CXX_FLAGS(-march=pentium,cpu_pentium,
			[CPU_FLAGS=-march=pentium])
			;;			  
	   	pentium2)
	   		ACX_CHECK_CXX_FLAGS(-march=pentium2,cpu_pentium2,
			[CPU_FLAGS=-march=pentium2])
	   		ACX_CHECK_CXX_FLAGS(-mmmx,mmmx,
			[CPU_FLAGS="$CPU_FLAGS -mmmx"])
			;;
	   	pentium3)
	   		ACX_CHECK_CXX_FLAGS(-march=pentium3,cpu_pentium3,
			[CPU_FLAGS=-march=pentium3])
	   		ACX_CHECK_CXX_FLAGS(-mmmx,mmmx,
			[CPU_FLAGS="$CPU_FLAGS -mmmx"])
			;;			  
	   	pentium4)
	   		ACX_CHECK_CXX_FLAGS(-march=pentium4,cpu_pentium4,
			[CPU_FLAGS=-march=pentium4])
                        ACX_CHECK_CXX_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CXX_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CXX_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	pentium4m)
	   		ACX_CHECK_CXX_FLAGS(-march=pentium4m,cpu_pentium4m,
			[CPU_FLAGS=-march=pentium4m])
                        ACX_CHECK_CXX_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CXX_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CXX_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;	
	   	prescott)
	   		ACX_CHECK_CXX_FLAGS(-march=prescott,cpu_prescott,
			[CPU_FLAGS=-march=prescott])
	   		ACX_CHECK_CXX_FLAGS(-msse,msse,
			[CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CXX_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CXX_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	Xeon*)
	   		ACX_CHECK_CXX_FLAGS(-march=pentium4,cpu_pentium4,
			[CPU_FLAGS=-march=pentium4])
                        ACX_CHECK_CXX_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_CXX_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_CXX_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
                core2)
                        ACX_CHECK_CXX_FLAGS(-mtune=core2,cpu_core2,[CPU_FLAGS="-mtune=core2"])
                        ;;
	   	k6)
	   		ACX_CHECK_CXX_FLAGS(-march=k6,cpu_k6,
			[CPU_FLAGS=-march=k6])
			;;			  
	   	athlon)
	   		ACX_CHECK_CXX_FLAGS(-march=athlon,cpu_athlon,
			[CPU_FLAGS=-march=athlon])
	   		ACX_CHECK_CXX_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   	athlon-xp)
	   		ACX_CHECK_CXX_FLAGS(-march=athlon-xp,cpu_athlon-xp,
			[CPU_FLAGS=-march=athlon-xp])
	   		ACX_CHECK_CXX_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   	Opteron*)
	   		ACX_CHECK_CXX_FLAGS(-march=athlon,cpu_athlon,
			[CPU_FLAGS=-march=athlon])
	   		ACX_CHECK_CXX_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   esac
	else
  		dnl More recent version
  		AX_GCC_ARCHFLAG([$portable], [CPU_FLAGS=$ax_cv_gcc_archflag],[echo "Did not find gcc flags"])
	fi
  fi

  if test -n "$CPU_OPTIM"; then
        CXXFLAGS="$CPU_OPTIM $CXXFLAGS"
  fi

  if test -n "$CPU_FLAGS"; then
        CXXFLAGS="$CXXFLAGS $CPU_FLAGS"
  fi

#  AC_SUBST(CODELET_OPTIM)

  if test -z "$CXXFLAGS"; then
	echo "" >> edm.log
	echo "**********************************************************" >> edm.log
        echo "* WARNING: Don't know the best CXXFLAGS for this system  *" >> edm.log
        echo "* Use  make CXXFLAGS=..., or edit the top level Makefile *" >> edm.log
	echo "* (otherwise, a default of CXXFLAGS=-O will be used)     *" >> edm.log
	echo "**********************************************************" >> edm.log
	echo "" >> edm.log
        CXXFLAGS="-O"
  fi

  ACX_CHECK_CXX_FLAGS(${CXXFLAGS}, guessed_cflags, , [
	echo "" >> edm.log
        echo "*********************************************************" >> edm.log
        echo "* WARNING: The guessed CXXFLAGS don't seem to work with *" >> edm.log
        echo "* your compiler.                                        *" >> edm.log
        echo "* Use make CXXFLAGS=..., or edit the top level Makefile *" >> edm.log
        echo "*********************************************************" >> edm.log
        echo "" >> edm.log
        CXXFLAGS=""
  ])

fi
])dnl
AC_DEFUN([ACX_CHECK_F77_FLAGS],
[
AC_REQUIRE([AC_PROG_F77])
ac_save_FFLAGS=$FFLAGS
FFLAGS="$1"
AC_CACHE_CHECK(whether $F77 accepts $1, ac_cv_$2,
               [AC_LINK_IFELSE([AC_LANG_PROGRAM()], [ac_cv_$2=yes], 
				[ac_cv_$2=no],	    [ac_cv_$2=no])])
FFLAGS=$ac_save_FFLAGS
if test "$ac_cv_$2" = yes; then
	:
	$3
else
	:
	$4
fi
])

AC_DEFUN([ACX_PROG_F77_MAXOPT],
[
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_PROG_F77])
AC_REQUIRE([ACX_PROG_CC_EGCS])
AC_REQUIRE([AC_CANONICAL_HOST])

# Try to determine "good" native compiler flags if none specified on command
# line
AC_LANG([Fortran 77])
if test "$ac_test_FFLAGS" != "set"; then
  FFLAGS=""
  ifort="no"
  case "${host_cpu}-${host_os}" in

  *linux*) if test "$F77" = ifc || test "$F77" = ifort ; then
                    FFLAGS="$WINTEL -align -pad -Vaxlib"
#                    FFLAGS="-align -pad -Vaxlib"
                    ifort="yes"
	        fi;;
  sparc-solaris2*) if test "$ac_compiler_gnu" != yes; then
                    FFLAGS="-native -fast -xO5 -dalign $WSUN"
                 fi;;

  alpha*-osf*)  if test  "$ac_compiler_gnu" != yes; then
                    FFLAGS="-newc $WALPHA -O5 -fp_reorder -tune host -arch host"
                fi;;

dnl Note: attempt to eliminate use of f77 unless f90 is the compiler
  hppa*-hpux*)  if test "$ac_compiler_gnu" != yes && "$F77" != "f90"; then
	            F77=fort77
                fi
  		    if test "$F77" = "f77"; then
	            F77=fort77
                fi
	          if test "$ac_compiler_gnu" != yes; then
                    FFLAGS="+U77 $WHPUX"
                fi;;

  *-sgi-irix*)	if test "$CC" = cc; then
		    CFLAGS="-O3 $WSGI"
		fi;;

   *-aix*)
	if test  "$ac_compiler_gnu" != yes; then
                ACX_CHECK_F77_FLAGS(-O3 $WAIX -qarch=auto -qtune=auto, qarch_auto,
                        [FFLAGS="-O3 $WAIX -qarch=auto -qtune=auto"]
                        [FFLAGS="-O3 -qansialias $WAIX"
                echo "*******************************************************"
                echo "*  You seem to have AIX and the IBM compiler.  It is  *"
                echo "*  recommended for best performance that you use:     *"
                echo "*                                                     *"
                echo "*  FFLAGS=-O3 -qarch=xxx -qtune=xxx -qansialias -w    *"
                echo "*                      ^^^        ^^^                 *"
                echo "*  where xxx is pwr2, pwr3, 604, or whatever kind of  *"
                echo "*  CPU you have.  (Set the FFLAGS environment var.    *"
                echo "*  and re-run configure.)  For more info, man f77.    *"
                echo "*******************************************************"])
        fi;;
  esac

  # use default flags for gcc on all systems
  if test $ac_cv_prog_gcc = yes && test "$ifort" = "no"; then
     FFLAGS="$WGCC"
  fi

  # the egcs scheduler is too smart and destroys our own schedule.
  # Disable the first instruction scheduling pass.  The second
  # scheduling pass (after register reload) is ok.
  if test "$acx_prog_egcs" = yes; then
     if test "$1" = fftw; then
        FFLAGS="$FFLAGS -fno-schedule-insns"
     fi
  fi

  # test for gcc-specific flags:
  if test $ac_cv_prog_gcc = yes  && test "$ifort" = "no"; then
#     -malign-double for x86 systems
#   ACX_CHECK_F77_FLAGS(-malign-double,align_double,
#	FFLAGS="$FFLAGS -malign-double")
#     -fstrict-aliasing for gcc-2.95+
#    ACX_CHECK_F77_FLAGS(-fstrict-aliasing,fstrict_aliasing,
#	FFLAGS="$FFLAGS -fstrict-aliasing")
#   ACX_CHECK_F77_FLAGS(-mpreferred-stack-boundary=4, m_psb_4,
#       FFLAGS="$FFLAGS -mpreferred-stack-boundary=4")
    # -pipe ?
    ACX_CHECK_F77_FLAGS(-pipe,pipe,
        FFLAGS="$FFLAGS -pipe")
    # -fpermissive
    #ACX_CHECK_F77_FLAGS(-fpermissive,permissive,
    #    FFLAGS="$FFLAGS -fpermissive")
    # globals
    ACX_CHECK_F77_FLAGS(-fno-globals,[no_globals],
        FFLAGS="$FFLAGS -fno-globals")
    # unrolling
    ACX_CHECK_F77_FLAGS(-funroll-loops,[unroll_loops],
        FFLAGS="$FFLAGS -funroll-loops")

  fi
# test if the compiler accepts static
  if test "$static" = "yes";then
    ACX_CHECK_F77_FLAGS(-static, m_static_f77,FFLAGS="$FFLAGS -static")
  fi
  CPU_FLAGS=""
 ACX_CHECK_F77_FLAGS(-march=native,cpu_native,
                        [CPU_FLAGS=-march=native])
  CPU_OPTIM=""
  CODELET_OPTIM=""
  if test "$GCC" = "yes" && test "$ifort" = "no"; then
          if  test "$ifort" = "no" ; then
                  CPU_OPTIM=-O3
                  CODELET_OPTIM=-O3
          fi
	  dnl try to guess correct CPU flags, at least for linux
	  case "${host_cpu}" in
	  i586*)  ACX_CHECK_F77_FLAGS(-march=pentium,cpu_pentium,
			[CPU_FLAGS=-march=pentium],
			[ACX_CHECK_F77_FLAGS(-mpentium,pentium,
				[CPU_FLAGS=-mpentium])])
		  if test "$1" = fftw; then
	            CODELET_OPTIM=-O
                  fi
		  if test "$1" = benchfft; then
	            CPU_OPTIM=-O2
                  fi
		  ;;
	  i686*)  ACX_CHECK_F77_FLAGS(-march=native,cpu_native,
			[CPU_FLAGS=-march=native])
                 if test "$1" = fftw; then
	            CODELET_OPTIM=-O
                  fi
		  if test "$1" = benchfft; then
	            CPU_OPTIM=-O2
                  fi
		  ;;
	  sparc*)
dnl                   ACX_CHECK_F77_FLAGS(-march=ultrasparc,cpu_ultrasparc,
dnl			[CPU_FLAGS=-march=ultrasparc])
		  ;;
	  alphaev67)  ACX_CHECK_F77_FLAGS(-march=ev67,cpu_ev67,
			[CPU_FLAGS=-march=ev67])
		  ;;
	  alphaev6)  ACX_CHECK_F77_FLAGS(-march=ev6,cpu_ev6,
			[CPU_FLAGS=-march=ev6])
		  ;;
	  alphaev56)  ACX_CHECK_F77_FLAGS(-march=ev56,cpu_ev56,
			[CPU_FLAGS=-march=ev56],
			[ACX_CHECK_F77_FLAGS(-march=ev5,cpu_ev5,
				[CPU_FLAGS=-march=ev5])])
		  ;;
	  alphaev5)  ACX_CHECK_F77_FLAGS(-march=ev5,cpu_ev5,
			[CPU_FLAGS=-march=ev5])
		  ;;
             hppa*)  ACX_CHECK_F77_FLAGS(-march=2.0,,
                        [CPU_FLAGS=-march=2.0])          
                  ;;
	  powerpc*)
		cputype=`((grep cpu /proc/cpuinfo | head -1 | cut -d: -f2 | sed 's/ //g') ; /usr/bin/machine ; /bin/machine) 2> /dev/null`
		cputype=`echo $cputype | sed -e s/ppc//g`
		is60x=`echo $cputype | egrep "^60[[0-9]]e?$"`
		is750=`echo $cputype | grep "750"`
		is74xx=`echo $cputype | egrep "^74[[0-9]][[0-9]]$"`
		if test -n "$is60x"; then
			ACX_CHECK_F77_FLAGS(-march=$cputype,m_cpu_60x,
				CPU_FLAGS=-march=$cputype)
		elif test -n "$is750"; then
			ACX_CHECK_F77_FLAGS(-march=750,m_cpu_750,
				CPU_FLAGS=-march=750)
		elif test -n "$is74xx"; then
			ACX_CHECK_F77_FLAGS(-march=$cputype,m_cpu_74xx,
				CPU_FLAGS=-march=$cputype)
		fi
		if test -z "$CPU_FLAGS"; then
		        ACX_CHECK_F77_FLAGS(-march=powerpc,m_cpu_powerpc,
				CPU_FLAGS=-march=powerpc)
		fi
		if test -z "$CPU_FLAGS"; then
			ACX_CHECK_F77_FLAGS(-mpowerpc,m_powerpc,
				CPU_FLAGS=-mpowerpc)
		fi
	  esac
# Added material if wincpu has been defined
	if test "$wincpu" != "None"; then
	   CPU_FLAGS="-march=$wincpu"
	   case "$wincpu" in
		portable)
	   		ACX_CHECK_F77_FLAGS(-mfpmath=387,math_387,
			[CPU_FLAGS="$CPU_FLAGS -mfpmath=387"])
			;;			  	   		
	   	i586)
	   		ACX_CHECK_F77_FLAGS(-march=i586,cpu_i586,
			[CPU_FLAGS=-march=i586])
			;;			  	   
	   	pentium)
	   		ACX_CHECK_F77_FLAGS(-march=pentium,cpu_pentium,
			[CPU_FLAGS=-march=pentium])
			;;			  
	   	pentium2)
	   		ACX_CHECK_F77_FLAGS(-march=pentium2,cpu_pentium2,
			[CPU_FLAGS=-march=pentium2])
	   		ACX_CHECK_F77_FLAGS(-mmmx,mmmx,
			[CPU_FLAGS="$CPU_FLAGS -mmmx"])
			;;
	   	pentium3)
	   		ACX_CHECK_F77_FLAGS(-march=pentium3,cpu_pentium3,
			[CPU_FLAGS=-march=pentium3])
	   		ACX_CHECK_F77_FLAGS(-mmmx,mmmx,
			[CPU_FLAGS="$CPU_FLAGS -mmmx"])
			;;			  
	   	pentium4)
	   		ACX_CHECK_F77_FLAGS(-march=pentium4,cpu_pentium4,
			[CPU_FLAGS=-march=pentium4])
                        ACX_CHECK_F77_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_F77_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_F77_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	pentium4m)
	   		ACX_CHECK_F77_FLAGS(-march=pentium4m,cpu_pentium4m,
			[CPU_FLAGS=-march=pentium4m])
                        ACX_CHECK_F77_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_F77_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_F77_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;	
	   	prescott)
	   		ACX_CHECK_F77_FLAGS(-march=prescott,cpu_prescott,
			[CPU_FLAGS=-march=prescott])
	   		ACX_CHECK_F77_FLAGS(-msse,msse,
			[CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_F77_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_F77_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])
			;;			  
	   	Xeon*)
	   		ACX_CHECK_F77_FLAGS(-march=pentium4,cpu_pentium4,
			[CPU_FLAGS=-march=pentium4])
                        ACX_CHECK_F77_FLAGS(-msse,msse,
                        [CPU_FLAGS="$CPU_FLAGS -msse"])
                        ACX_CHECK_F77_FLAGS(-msse2,msse2,
                        [CPU_FLAGS="$CPU_FLAGS -msse2"])
                        ACX_CHECK_F77_FLAGS(-mfpmath=sse,mfpmath,
                        [CPU_FLAGS="$CPU_FLAGS -mfpmath=sse"])                        
			;;			  
                core2)
                        ACX_CHECK_F77_FLAGS(-mtune=core2,cpu_core2,[CPU_FLAGS="-mtune=core2"])
                        ;;
	   	k6)
	   		ACX_CHECK_F77_FLAGS(-march=k6,cpu_k6,
			[CPU_FLAGS=-march=k6])
			;;			  
	   	athlon)
	   		ACX_CHECK_F77_FLAGS(-march=athlon,cpu_athlon,
			[CPU_FLAGS=-march=athlon])
	   		ACX_CHECK_F77_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   	athlon-xp)
	   		ACX_CHECK_F77_FLAGS(-march=athlon-xp,cpu_athlon-xp,
			[CPU_FLAGS=-march=athlon-xp])
	   		ACX_CHECK_F77_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   	Opteron*)
	   		ACX_CHECK_F77_FLAGS(-march=athlon,cpu_athlon,
			[CPU_FLAGS=-march=athlon])
	   		ACX_CHECK_F77_FLAGS(-m3dnow,m3dnow,
			[CPU_FLAGS="$CPU_FLAGS -m3dnow"])
			;;			  
	   esac
	else
  		dnl More recent version
  		AX_GCC_ARCHFLAG([$portable], [CPU_FLAGS=$ax_cv_gcc_archflag],[echo "Did not find gcc flags"])
	fi
  fi

  if test -n "$CPU_OPTIM" && test "$ifort" = "no"; then
        FFLAGS="$CPU_OPTIM $FFLAGS"
  fi

  if test -n "$CPU_FLAGS" && test "$ifort" = "no"; then
        FFLAGS="$FFLAGS $CPU_FLAGS"
  fi

#  AC_SUBST(CODELET_OPTIM)

  if test -z "$FFLAGS"; then
	echo "" >> edm.log
	echo "********************************************************" >> edm.log
        echo "* WARNING: Don't know the best FFLAGS for this system  *" >> edm.log
        echo "* Use  make FFLAGS=..., or edit the top level Makefile *" >> edm.log
	echo "* (otherwise, a default of FFLAGS=-O will be used)     *" >> edm.log
	echo "********************************************************" >> edm.log
	echo "" >> edm.log
        FFLAGS="-O"
  fi

  ACX_CHECK_F77_FLAGS(${FFLAGS}, guessed_cflags, , [
	echo "" >> edm.log
        echo "********************************************************" >> edm.log
        echo "* WARNING: The guessed FFLAGS don't seem to work with  *" >> edm.log
        echo "* your compiler.                                       *" >> edm.log
        echo "* Use  make FFLAGS=..., or edit the top level Makefile *" >> edm.log
        echo "********************************************************" >> edm.log
        echo "" >> edm.log
        FFLAGS=""
  ])

fi
])dnl
AC_DEFUN([CHECK_WITH_CYGWIN],
[
  case $host_os in
  cygwin* | mingw* | pw32* )
    if test "$GCC" = yes; then
      # Ensure MSVC-compatible struct packing convention.
      # Depends on GCC version. gcc2 uses -fnative-struct while
      # gcc3 uses -mms-bitfields.
      #
      msnative_struct=''
      AC_MSG_CHECKING([how to get MSVC-compatible struct packing])
      case `$CC --version | sed -e 's,\..*,.,' -e q` in
      2.)
        if $CC -v --help 2>/dev/null | grep fnative-struct > /dev/null; then
        msnative_struct='-fnative-struct'
        fi
        ;;
      *)
        if $CC -v --help 2>/dev/null | grep ms-bitfields > /dev/null; then
        msnative_struct='-mms-bitfields'
        fi
        ;;
      esac

      if test x"$msnative_struct" = x; then
        AC_MSG_RESULT([no way])
        AC_MSG_WARN([produced libraries might be incompatible with MSVC libs])
      else
        CXXFLAGS="$CXXFLAGS $msnative_struct"
	CFLAGS="$CFLAGS $msnative_struct"
	FFLAGS="$FFLAGS $msnative_struct"
        AC_MSG_RESULT([${msnative_struct}])
      fi
    fi

    # Export all symbols to Win32 DLL using MinGW 2.0 ld.
    WIN32_LD_EXPORT_ALL_SYMBOLS=''
    AC_MSG_CHECKING([whether ld accepts --export-all-symbols])
    if $LD --help 2>&1 | egrep 'export-all-symbols' > /dev/null; then
      WIN32_LD_EXPORT_ALL_SYMBOLS='-Wl,--export-all-symbols'
      AC_MSG_RESULT([yes])
    else
      AC_MSG_RESULT([no])
    fi
    if test x"$WIN32_LD_EXPORT_ALL_SYMBOLS" != x; then
      LDFLAGS="$LDFLAGS $WIN32_LD_EXPORT_ALL_SYMBOLS"
    fi
    ;;
  esac
])
AC_DEFUN([ACX_PROG_GCC_VERSION],
[
AC_REQUIRE([AC_PROG_CC])
AC_CACHE_CHECK(whether we are using gcc $1.$2 or later, ac_cv_prog_gcc_$1_$2,
[
dnl The semicolon after "yes" below is to pacify NeXT's syntax-checking cpp.
cat > conftest.c <<EOF
#ifdef __GNUC__
#  if (__GNUC__ > $1) || (__GNUC__ == $1 && __GNUC_MINOR__ >= $2)
     yes;
#  endif
#endif
EOF
if AC_TRY_COMMAND(${CC-cc} -E conftest.c) | egrep yes >/dev/null 2>&1; then
  ac_cv_prog_gcc_$1_$2=yes
else
  ac_cv_prog_gcc_$1_$2=no
fi
])
if test "$ac_cv_prog_gcc_$1_$2" = yes; then
	:
	$3
else
	:
	$4
fi
])

AC_DEFUN([ACX_PROG_CC_EGCS],
[ACX_PROG_GCC_VERSION(2,90,acx_prog_egcs=yes,acx_prog_egcs=no)])

# Check to see if we are using a version of gcc that aligns the stack
# (true in gcc-2.95+, which have the -mpreferred-stack-boundary flag).
# Also check for stack alignment bug in gcc-2.95.x
# (see http://egcs.cygnus.com/ml/gcc-bugs/1999-11/msg00259.html), and
# whether main() is correctly aligned by the OS/libc/loader.
AC_DEFUN([ACX_GCC_ALIGNS_STACK],
[
AC_REQUIRE([AC_PROG_CC])
acx_gcc_aligns_stack=no
if test "$GCC" = "yes"; then
ACX_CHECK_CC_FLAGS(-mpreferred-stack-boundary=4, m_pref_stack_boundary_4)
if test "$ac_m_pref_stack_boundary_4" = "yes"; then
	AC_MSG_CHECKING([whether the stack is correctly aligned by gcc])
	save_CFLAGS="$CFLAGS"
	CFLAGS="-O -malign-double"
	AC_TRY_RUN([#include <stdlib.h>
#       include <stdio.h>
	struct yuck { int blechh; };
	int one(void) { return 1; }
	struct yuck ick(void) { struct yuck y; y.blechh = 3; return y; }
#       define CHK_ALIGN(x) if ((((long) &(x)) & 0x7)) { fprintf(stderr, "bad alignment of " #x "\n"); exit(1); }
	void blah(int foo) { double foobar; CHK_ALIGN(foobar); }
	int main2(void) {double ok1; struct yuck y; double ok2; CHK_ALIGN(ok1);
                         CHK_ALIGN(ok2); y = ick(); blah(one()); return 0;}
	int main(void) { if ((((long) (__builtin_alloca(0))) & 0x7)) __builtin_alloca(4); return main2(); }
	], [acx_gcc_aligns_stack=yes; acx_gcc_stack_align_bug=no], 
	acx_gcc_stack_align_bug=yes, acx_gcc_stack_align_bug=yes)
	CFLAGS="$save_CFLAGS"
	AC_MSG_RESULT($acx_gcc_aligns_stack)
fi
fi
if test "$acx_gcc_aligns_stack" = yes; then
	:
	$1
else
	:
	$2
fi
])
 
dnl @synopsis AX_GCC_ARCHFLAG([PORTABLE?], [ACTION-SUCCESS], [ACTION-FAILURE])
dnl
dnl @summary find target architecture name for gcc -march/-mtune flags
dnl
dnl This macro tries to guess the "native" arch corresponding to the
dnl target architecture for use with gcc's -march=arch or -mtune=arch
dnl flags. If found, the cache variable $ax_cv_gcc_archflag is set to
dnl this flag and ACTION-SUCCESS is executed; otherwise
dnl $ax_cv_gcc_archflag is is set to "unknown" and ACTION-FAILURE is
dnl executed. The default ACTION-SUCCESS is to add $ax_cv_gcc_archflag
dnl to the end of $CFLAGS.
dnl
dnl PORTABLE? should be either [yes] (default) or [no]. In the former
dnl case, the flag is set to -mtune (or equivalent) so that the
dnl architecture is only used for tuning, but the instruction set used
dnl is still portable. In the latter case, the flag is set to -march
dnl (or equivalent) so that architecture-specific instructions are
dnl enabled.
dnl
dnl The user can specify --with-gcc-arch=<arch> in order to override
dnl the macro's choice of architecture, or --without-gcc-arch to
dnl disable this.
dnl
dnl When cross-compiling, or if $CC is not gcc, then ACTION-FAILURE is
dnl called unless the user specified --with-gcc-arch manually.
dnl
dnl Requires macros: AX_CHECK_COMPILER_FLAGS, AX_GCC_X86_CPUID
dnl
dnl (The main emphasis here is on recent CPUs, on the principle that
dnl doing high-performance computing on old hardware is uncommon.)
dnl
dnl @category Misc
dnl @author Steven G. Johnson <stevenj@alum.mit.edu> and Matteo Frigo.
dnl @version 2006-01-04
dnl @license GPLWithACException

AC_DEFUN([AX_GCC_ARCHFLAG],
[AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_CANONICAL_HOST])

AC_ARG_WITH(gcc-arch, [AC_HELP_STRING([--with-gcc-arch=<arch>], [use architecture <arch> for gcc -march/-mtune, instead of guessing])],
	ax_gcc_arch=$withval, ax_gcc_arch=yes)

AC_MSG_CHECKING([for gcc architecture flag])
AC_MSG_RESULT([])
AC_CACHE_VAL(ax_cv_gcc_archflag,
[
ax_cv_gcc_archflag="unknown"

if test "$GCC" = yes; then

if test "x$ax_gcc_arch" = xyes; then
ax_gcc_arch=""
if test "$cross_compiling" = no; then
case $host_cpu in
  i[[3456]]86*|x86_64*) # use cpuid codes, in part from x86info-1.7 by D. Jones
     AX_GCC_X86_CPUID(0)
     AX_GCC_X86_CPUID(1)
     case $ax_cv_gcc_x86_cpuid_0 in
       *:756e6547:*:*) # Intel
          case $ax_cv_gcc_x86_cpuid_1 in
	    *5[[48]]?:*:*:*) ax_gcc_arch="native pentium-mmx pentium" ;;
	    *5??:*:*:*) ax_gcc_arch="native pentium" ;;
	    *6[[3456]]?:*:*:*) ax_gcc_arch="native pentium2 pentiumpro" ;;
	    *6a?:*[[01]]:*:*) ax_gcc_arch="native pentium2 pentiumpro" ;;
	    *6a?:*[[234]]:*:*) ax_gcc_arch="native pentium3 pentiumpro" ;;
	    *6[[9d]]?:*:*:*) ax_gcc_arch="native pentium-m pentium3 pentiumpro" ;;
	    *6[[78b]]?:*:*:*) ax_gcc_arch="native pentium3 pentiumpro" ;;
	    *6??:*:*:*) ax_gcc_arch="native pentiumpro" ;;
            *f3[[347]]:*:*:*|*f4[1347]:*:*:*)
		case $host_cpu in
                  x86_64*) ax_gcc_arch="native nocona pentium4 pentiumpro" ;;
                  *) ax_gcc_arch="native prescott pentium4 pentiumpro" ;;
                esac ;;
            *f??:*:*:*) ax_gcc_arch="native pentium4 pentiumpro";;
          esac ;;
       *:68747541:*:*) # AMD
          case $ax_cv_gcc_x86_cpuid_1 in
	    *5[[67]]?:*:*:*) ax_gcc_arch="native k6" ;;
	    *5[[8d]]?:*:*:*) ax_gcc_arch="native k6-2 k6" ;;
	    *5[[9]]?:*:*:*) ax_gcc_arch="native k6-3 k6" ;;
	    *60?:*:*:*) ax_gcc_arch="native k7" ;;
	    *6[[12]]?:*:*:*) ax_gcc_arch="native athlon k7" ;;
	    *6[[34]]?:*:*:*) ax_gcc_arch="native athlon-tbird k7" ;;
	    *67?:*:*:*) ax_gcc_arch="native athlon-4 athlon k7" ;;
	    *6[[68a]]?:*:*:*)
	       AX_GCC_X86_CPUID(0x80000006) # L2 cache size
	       case $ax_cv_gcc_x86_cpuid_0x80000006 in
                 *:*:*[[1-9a-f]]??????:*) # (L2 = ecx >> 16) >= 256
			ax_gcc_arch="native athlon-xp athlon-4 athlon k7" ;;
                 *) ax_gcc_arch="native athlon-4 athlon k7" ;;
	       esac ;;
	    *f[[4cef8b]]?:*:*:*) ax_gcc_arch="native athlon64 k8" ;;
	    *f5?:*:*:*) ax_gcc_arch="native opteron k8" ;;
	    *f7?:*:*:*) ax_gcc_arch="native athlon-fx opteron k8" ;;
	    *f??:*:*:*) ax_gcc_arch="native k8" ;;
          esac ;;
	*:746e6543:*:*) # IDT
	   case $ax_cv_gcc_x86_cpuid_1 in
	     *54?:*:*:*) ax_gcc_arch="native winchip-c6" ;;
	     *58?:*:*:*) ax_gcc_arch="native winchip2" ;;
	     *6[[78]]?:*:*:*) ax_gcc_arch="native c3" ;;
	     *69?:*:*:*) ax_gcc_arch="native c3-2 c3" ;;
	   esac ;;
     esac
     if test x"$ax_gcc_arch" = x; then # fallback
	case $host_cpu in
	  i586*) ax_gcc_arch="native pentium" ;;
	  i686*) ax_gcc_arch="native pentiumpro" ;;
        esac
     fi
     ;;

  sparc*)
     AC_PATH_PROG([PRTDIAG], [prtdiag], [prtdiag], [$PATH:/usr/platform/`uname -i`/sbin/:/usr/platform/`uname -m`/sbin/])
     cputype=`(((grep cpu /proc/cpuinfo | cut -d: -f2) ; ($PRTDIAG -v |grep -i sparc) ; grep -i cpu /var/run/dmesg.boot ) | head -n 1) 2> /dev/null`
     cputype=`echo "$cputype" | tr -d ' -' |tr $as_cr_LETTERS $as_cr_letters`
     case $cputype in
         *ultrasparciv*) ax_gcc_arch="native ultrasparc4 ultrasparc3 ultrasparc v9" ;;
         *ultrasparciii*) ax_gcc_arch="native ultrasparc3 ultrasparc v9" ;;
         *ultrasparc*) ax_gcc_arch="native ultrasparc v9" ;;
         *supersparc*|*tms390z5[[05]]*) ax_gcc_arch="native supersparc v8" ;;
         *hypersparc*|*rt62[[056]]*) ax_gcc_arch="natve hypersparc v8" ;;
         *cypress*) ax_gcc_arch="native cypress" ;;
     esac ;;

  alphaev5) ax_gcc_arch="native ev5" ;;
  alphaev56) ax_gcc_arch="native ev56" ;;
  alphapca56) ax_gcc_arch="native pca56 ev56" ;;
  alphapca57) ax_gcc_arch="native pca57 pca56 ev56" ;;
  alphaev6) ax_gcc_arch="native ev6" ;;
  alphaev67) ax_gcc_arch="native ev67" ;;
  alphaev68) ax_gcc_arch="native ev68 ev67" ;;
  alphaev69) ax_gcc_arch="native ev69 ev68 ev67" ;;
  alphaev7) ax_gcc_arch="native ev7 ev69 ev68 ev67" ;;
  alphaev79) ax_gcc_arch="native ev79 ev7 ev69 ev68 ev67" ;;

  powerpc*)
     cputype=`((grep cpu /proc/cpuinfo | head -n 1 | cut -d: -f2 | cut -d, -f1 | sed 's/ //g') ; /usr/bin/machine ; /bin/machine; grep CPU /var/run/dmesg.boot | head -n 1 | cut -d" " -f2) 2> /dev/null`
     cputype=`echo $cputype | sed -e 's/ppc//g;s/ *//g'`
     case $cputype in
       *750*) ax_gcc_arch="native 750 G3" ;;
       *740[[0-9]]*) ax_gcc_arch="$cputype 7400 G4" ;;
       *74[[4-5]][[0-9]]*) ax_gcc_arch="$cputype 7450 G4" ;;
       *74[[0-9]][[0-9]]*) ax_gcc_arch="$cputype G4" ;;
       *970*) ax_gcc_arch="970 G5 power4";;
       *POWER4*|*power4*|*gq*) ax_gcc_arch="power4 970";;
       *POWER5*|*power5*|*gr*|*gs*) ax_gcc_arch="power5 power4 970";;
       603ev|8240) ax_gcc_arch="$cputype 603e 603";;
       *) ax_gcc_arch=$cputype ;;
     esac
     ax_gcc_arch="$ax_gcc_arch powerpc"
     ;;
esac
fi # not cross-compiling
fi # guess arch

if test "x$ax_gcc_arch" != x -a "x$ax_gcc_arch" != xno; then
for arch in $ax_gcc_arch; do
  if test "x[]m4_default([$1],yes)" = xyes; then # if we require portable code
    flags="-mtune=$arch"
    # -mcpu=$arch and m$arch generate nonportable code on every arch except
    # x86.  And some other arches (e.g. Alpha) don't accept -mtune.  Grrr.
    case $host_cpu in i*86|x86_64*) flags="$flags -mcpu=$arch -m$arch";; esac
  else
    flags="-march=$arch -mcpu=$arch -m$arch"
  fi
  for flag in $flags; do
    AX_CHECK_COMPILER_FLAGS($flag, [ax_cv_gcc_archflag=$flag; break])
  done
  test "x$ax_cv_gcc_archflag" = xunknown || break
done
fi

fi # $GCC=yes
])
AC_MSG_CHECKING([for gcc architecture flag])
AC_MSG_RESULT($ax_cv_gcc_archflag)
if test "x$ax_cv_gcc_archflag" = xunknown; then
  m4_default([$3],:)
else
  m4_default([$2], [CFLAGS="$CFLAGS $ax_cv_gcc_archflag"])
fi
])
dnl @synopsis AX_GCC_X86_CPUID(OP)
dnl
dnl @summary run x86 cpuid instruction OP using gcc inline assembler
dnl
dnl On Pentium and later x86 processors, with gcc or a compiler that
dnl has a compatible syntax for inline assembly instructions, run a
dnl small program that executes the cpuid instruction with input OP.
dnl This can be used to detect the CPU type.
dnl
dnl On output, the values of the eax, ebx, ecx, and edx registers are
dnl stored as hexadecimal strings as "eax:ebx:ecx:edx" in the cache
dnl variable ax_cv_gcc_x86_cpuid_OP.
dnl
dnl If the cpuid instruction fails (because you are running a
dnl cross-compiler, or because you are not using gcc, or because you
dnl are on a processor that doesn't have this instruction),
dnl ax_cv_gcc_x86_cpuid_OP is set to the string "unknown".
dnl
dnl This macro mainly exists to be used in AX_GCC_ARCHFLAG.
dnl
dnl @category Misc
dnl @author Steven G. Johnson <stevenj@alum.mit.edu> and Matteo Frigo.
dnl @version 2005-05-30
dnl @license GPLWithACException

AC_DEFUN([AX_GCC_X86_CPUID],
[AC_REQUIRE([AC_PROG_CC])
AC_LANG_PUSH([C])
AC_CACHE_CHECK(for x86 cpuid $1 output, ax_cv_gcc_x86_cpuid_$1,
 [AC_RUN_IFELSE([AC_LANG_PROGRAM([#include <stdio.h>], [
     int op = $1, eax, ebx, ecx, edx;
     FILE *f;
      __asm__("cpuid"
        : "=a" (eax), "=b" (ebx), "=c" (ecx), "=d" (edx)
        : "a" (op));
     f = fopen("conftest_cpuid", "w"); if (!f) return 1;
     fprintf(f, "%x:%x:%x:%x\n", eax, ebx, ecx, edx);
     fclose(f);
     return 0;
])], 
     [ax_cv_gcc_x86_cpuid_$1=`cat conftest_cpuid`; rm -f conftest_cpuid],
     [ax_cv_gcc_x86_cpuid_$1=unknown; rm -f conftest_cpuid],
     [ax_cv_gcc_x86_cpuid_$1=unknown])])
AC_LANG_POP([C])
])
dnl @synopsis AX_CHECK_COMPILER_FLAGS(FLAGS, [ACTION-SUCCESS], [ACTION-FAILURE])
dnl
dnl @summary check whether FLAGS are accepted by the compiler
dnl
dnl Check whether the given compiler FLAGS work with the current
dnl language's compiler, or whether they give an error. (Warnings,
dnl however, are ignored.)
dnl
dnl ACTION-SUCCESS/ACTION-FAILURE are shell commands to execute on
dnl success/failure.
dnl
dnl @category Misc
dnl @author Steven G. Johnson <stevenj@alum.mit.edu> and Matteo Frigo.
dnl @version 2005-05-30
dnl @license GPLWithACException

AC_DEFUN([AX_CHECK_COMPILER_FLAGS],
[AC_PREREQ(2.59) dnl for _AC_LANG_PREFIX
AC_MSG_CHECKING([whether _AC_LANG compiler accepts $1])
dnl Some hackery here since AC_CACHE_VAL can't handle a non-literal varname:
AS_LITERAL_IF([$1],
  [AC_CACHE_VAL(AS_TR_SH(ax_cv_[]_AC_LANG_ABBREV[]_flags_$1), [
      ax_save_FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
      _AC_LANG_PREFIX[]FLAGS="$1"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], 
        AS_TR_SH(ax_cv_[]_AC_LANG_ABBREV[]_flags_$1)=yes,
        AS_TR_SH(ax_cv_[]_AC_LANG_ABBREV[]_flags_$1)=no)
      _AC_LANG_PREFIX[]FLAGS=$ax_save_FLAGS])],
  [ax_save_FLAGS=$[]_AC_LANG_PREFIX[]FLAGS
   _AC_LANG_PREFIX[]FLAGS="$1"
   AC_COMPILE_IFELSE([AC_LANG_PROGRAM()], 
     eval AS_TR_SH(ax_cv_[]_AC_LANG_ABBREV[]_flags_$1)=yes,
     eval AS_TR_SH(ax_cv_[]_AC_LANG_ABBREV[]_flags_$1)=no)
   _AC_LANG_PREFIX[]FLAGS=$ax_save_FLAGS])
eval ax_check_compiler_flags=$AS_TR_SH(ax_cv_[]_AC_LANG_ABBREV[]_flags_$1)
AC_MSG_RESULT($ax_check_compiler_flags)
if test "x$ax_check_compiler_flags" = xyes; then
	m4_default([$2], :)
else
	m4_default([$3], :)
fi
])dnl AX_CHECK_COMPILER_FLAGS
