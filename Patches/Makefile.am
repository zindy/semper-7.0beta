bin_PROGRAMS = LinkHelp
noinst_PROGRAMS = TSemp
# Try and prevent Mac from messing up gfortran compilation with -R
LIBS =

LinkHelp_SOURCES = LinkHelp.f

noinst_LIBRARIES = libsPatches.a

libsPatches_a_SOURCES =  SempBuild.c IsMotorola.f helpfl6.f
libsPatches_a_LIBADD  =  $(ALLOCA) $(LIBOBJS)

INCLUDES = -DBUSER=\"$(USER)\" -DBHOST=\"$(HOSTNAME)\" \
        -DHOST_TRIPLET=\"$(host_triplet)\" -DF77=\"$(F77)\" -DCC=\"$(CC)\" \
        -DBROWSER=\"$(BROWSER)\" -DBROWSERF=\"$(BROWSERF)\" -DPATCHES=\"$(PATCHES)\" \
        -DPREFIX=\"$(prefix)\" 
#        -DPREFIX=\"$(prefix)\" -DBTIME=\"$(BTIME)\"

# Testcode
TSemp_SOURCES = TSempBuild.f SempBuild.c

test-build:  TSemp
	./TSemp

#Ensure that everything relevant is added to distribution
EXTRA_DIST = access.f     getcwdf.c    lnblnk1.f    TSempBuild.f  \
	     accessf.c    lnblnk2.f    malloc.c     usleep.c  helpfl6.f \
	     lnblnk.c     realloc.c    filpos_v1.f  IsMotorola.f  SempBuild.c \
	     filpos_v2.f  LinkHelp.f   

Patchdir = $(prefix)/bin
dist_Patch_DATA = Helpcom.sh

