ls > t1
wc -l t1 > tt ; read nlin rest < tt 

echo > Makefile.am
echo INCLUDE = ../Includes >> Makefile.am
echo >> Makefile.am
echo lib_LTLIBRARIES = libs$1.la >> Makefile.am
echo >> Makefile.am
echo libs$1_la_SOURCES =  >> Makefile.am
head -$nlin ../Tmp/Slash > t2
paste t1 t2 >> Makefile.am
rm t1 t2 tt

