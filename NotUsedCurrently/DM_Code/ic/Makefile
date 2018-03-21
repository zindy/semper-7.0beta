# Makefile for compiling ic.f image conversion program

# Uncomment the correct line begining with fflags =

# For ic on macos x
# make |& fgrep -v 'unused variable' to ignore unused variable warnings
# -Wall = -Wunused -Wuninitialized
#fflags = -Ddarwin -O -fno-underscoring -fbounds-check -Wuninitialized -Wimplicit

# For ic on Silicon Graphics
#fflags = -Dirix -C

# for ic on linux
fflags = -Dlinux -O -fno-underscoring -fbounds-check -Wuninitialized -Wimplicit


objects = ic.o syscalls.o bits.o misc.o dm2.o dm3.o elp.o s6dsk.o s6rwb.o an10000.o

ic :	$(objects)
	f77 -o ic $(objects)

ic.o :	ic.f vars.h
	f77 -c ic.f $(fflags)

syscalls.o : syscalls.F
	f77 -c syscalls.F $(fflags)

bits.o : bits.f vars.h cdate.h
	f77 -c bits.f $(fflags)

misc.o : misc.f
	f77 -c misc.f $(fflags)

dm2.o : dm2.f vars.h
	f77 -c dm2.f $(fflags)

dm3.o : dm3.f vars.h
	f77 -c dm3.f $(fflags)

elp.o : elp.f vars.h
	f77 -c elp.f $(fflags)

s6dsk.o : s6dsk.f vars.h
	f77 -c s6dsk.f $(fflags)

s6rwb.o : s6rwb.f vars.h
	f77 -c s6rwb.f $(fflags)

an10000.o : an10000.f vars.h
	f77 -c an10000.f $(fflags)

cdate.h : vars.h ic.f syscalls.F bits.f misc.f dm2.f dm3.f elp.f s6dsk.f s6rwb.f an10000.f
	echo "      cdate = '`date '+%a %e %b %Y %H:%M:%S %Z'`'" > cdate.h

clean:
	rm -f *.o

distclean: clean
	rm -f ic cdate.h ic.exe
