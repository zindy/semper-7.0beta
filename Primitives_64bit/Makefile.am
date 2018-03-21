

#noinst_PROGRAMS = a1conv_test cform_test mctime_test mcdc61_test
noinst_PROGRAMS = cform_test mctime_test mcdc61_test a1conv_test

a1conv_test_SOURCES = a1conv_test.f
cform_test_SOURCES  = cform_test.f cform.c
mctime_test_SOURCES = mctime_test.f
mcdc61_test_SOURCES = mcdc61_test.f

a1conv_test_LDADD = @SLIB_ADD@
mctime_test_LDADD = @SLIB_ADD@
mcdc61_test_LDADD = @SLIB_ADD@

a1conv_test_LDFLAGS = @SLIB_LOC@
mctime_test_LDFLAGS = @SLIB_LOC@
mcdc61_test_LDFLAGS = @SLIB_LOC@

test-a1conv: a1conv_test
	./a1conv_test
test-cform:  cform_test
	./cform_test
test-mctime: mctime_test
	./mctime_test
test-mcdc61: mcdc61_test
	./mcdc61_test

noinst_LIBRARIES = libsPrimitives.a

EXTRA_DIST = dcache.h  events.h  icset.h fsbuff.h

libsPrimitives_a_SOURCES = \
a1conv.c \
bitfld.c \
bmap33.c \
ccache.c \
cdisc.c	\
cform.c	\
cunix.c	\
dcache.c \
eikio.c	\
fs.c \
gethid.c \
mcdc61.c \
mctime.c \
mctp61.c \
semhan.c \
stdio.c \
xrect.c	
#imgio is not standard c
#imgio.c 
#ipbase.c \
#ip12.c \
#ip4.c \
#ip5.c \
#ip6.c 
