#!/bin/csh

rm *.diff

foreach i ( */*.f */*.c */*.h *.c *.h *.f )
set base  = ` echo $i | sed -e 's/\.f//' -e 's/\.F//' `
set idiff = $i.diff
set other = $1

if ( -e $other/$i )then
 sdiff -ibs $i $other/$i > tmp.diff
 set sz = `wc -l tmp.diff | cut -f 1,1 -d" " `
 if !( $sz == "0") mv tmp.diff $idiff
endif

end
rm -f tmp.diff
