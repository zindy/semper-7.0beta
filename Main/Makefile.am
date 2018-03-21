# Main programs

bin_PROGRAMS = semgen7 semper7 helpman7 dskconv
semper7_SOURCES = semper.f 
semgen7_SOURCES = semgen.f
helpman7_SOURCES = helpman.f
dskconv_SOURCES = dskconv.c

semper7_LDFLAGS  = @SLIB_LOC@
semper7_LDADD    = @SLIB_ADD@
helpman7_LDFLAGS = @SLIB_LOC@
helpman7_LDADD   = @SLIB_ADD@ -lsPrimitives
semgen7_LDFLAGS  = @SLIB_LOC@
semgen7_LDADD    = @SLIB_ADD@ -lsPrimitives

semper7_DEPENDENCIES = ../Primitives/libsPrimitives.a  ../Commands/libsCommands.a \
                       ../Locals/libsLocals.a ../Middle/libsMiddle.a \
                       ../ProgramM/libsProgramM.a ../Patches/libsPatches.a \
                       ../UserInterface/libsUserInterface.a ../Graphics/libsGraphics.a \
                       ../Kernel/libsKernel.a

helpman7_DEPENDENCIES = ../Primitives/libsPrimitives.a  ../Commands/libsCommands.a \
                       ../Locals/libsLocals.a ../Middle/libsMiddle.a \
                       ../ProgramM/libsProgramM.a ../Patches/libsPatches.a \
                       ../UserInterface/libsUserInterface.a ../Graphics/libsGraphics.a \
                       ../Kernel/libsKernel.a

# Script for help in cygwin
dist_bin_SCRIPTS = cygshelp.sh

